-- |
-- Module      :  Handler.SubmitRelease
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Submit new release for consideration.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.SubmitRelease
  ( getSubmitReleaseR
  , postSubmitReleaseR )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust)
import Formatting (sformat, int)
import Import
import Path
import System.Directory (getCurrentDirectory)
import Yesod.Form.Bootstrap3
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T
import qualified Sound.HTagLib      as H
import qualified Svod               as S

-- | Information user must submit to upload something.

data SubmitReleaseForm = SubmitReleaseForm
  { srTitle   :: Text                  -- ^ Title of entire work (album)
  , srLicense :: License               -- ^ License of the work
  , srGenre   :: Maybe Text            -- ^ Genre (if any)
  , srYear    :: Int                   -- ^ Year of publication
  , srTracks  :: NonEmpty S.CreateFile -- ^ Tracks to create
  , srDemo    :: Bool                  -- ^ Is this a demo?
  , srDesc    :: Textarea              -- ^ Description of the release
  }

-- | Form for release submission.

submitReleaseForm :: Form SubmitReleaseForm
submitReleaseForm html = do
  maxy <- liftIO getCurrentYear
  let miny = maxy - 3
      form = SubmitReleaseForm
        <$> areq textField (withAutofocus $ bfs ("Название" :: Text)) Nothing
        <*> areq (selectFieldList licenses) (bfs ("Лицензия" :: Text)) Nothing
        <*> aopt textField (bfs ("Жанр" :: Text)) Nothing
        <*> areq (yearField miny maxy) (yearSettings miny maxy) (Just maxy)
        <*> areq tracksField (bfs ("Список записей" :: Text)) Nothing
        <*> areq checkBoxField (bfs ("Демо" :: Text)) (Just False)
        <*> areq textareaField (bfs ("Описание" :: Text)) Nothing
  renderBootstrap3 BootstrapBasicForm form html
  where licenses = (licensePretty &&& id) <$> [minBound..maxBound]

-- | Serve page with “submit release” form.

getSubmitReleaseR :: Handler Html
getSubmitReleaseR = do
  (form, enctype) <- generateFormPost submitReleaseForm
  uid <- fromJust <$> maybeAuthId
  serveSubmitRelease uid form enctype

-- | Process submission.

postSubmitReleaseR :: Handler Html
postSubmitReleaseR = do
  user <- fromJust <$> maybeAuth
  let uid       = entityKey user
      User {..} = entityVal user
  status <- runDB (S.canSubmitAgain uid)
  unless (status == S.CanSubmit) $
    permissionDenied "Вы не можете опубликовать что-либо сейчас."
  ((result, form), enctype) <- runFormPost submitReleaseForm
  case result of
    FormSuccess SubmitReleaseForm {..} -> do
      let rm = S.ReleaseMeta
            { rmArtist  = uid
            , rmAlbum   = H.mkAlbum  (T.unpack srTitle)
            , rmGenre   = H.mkGenre . T.unpack <$> srGenre
            , rmYear    = fromJust (H.mkYear srYear)
            , rmDesc    = unTextarea srDesc
            , rmLicense = srLicense
            , rmTracks  = srTracks }
      root  <- liftIO getCurrentDirectory >>= parseAbsDir
      sroot <- appStagingDir . appSettings <$> getYesod
      outcome <- runDB $ S.submitRelease (root </> sroot) rm srDemo
      case outcome of
        Left msg -> do
          setMsg MsgDanger (toHtml msg)
          serveSubmitRelease uid form enctype
        Right rid -> do
          releaseSlug <- getSlug . releaseSlug . fromJust <$> runDB (get rid)
          setMsg MsgSuccess [shamlet|
Публикация #
<strong>
  #{srTitle}
\ теперь ожидает рассмотрения. Пожалуйста будьте терпеливы, вы будете уведомлены
о принятом решении. Вы можете редактировать вашу работу пока она не
опубликована. В период рассмотрения администрация может посылать вам сообщения,
чтобы обсудить или поправить что-либо, пожалуйста реагируйте своевременно.
|]
          redirect (ReleaseR (getSlug userSlug) releaseSlug)
    _ -> serveSubmitRelease uid form enctype

-- | Serve “submit release” page.

serveSubmitRelease :: ToWidget App a
  => UserId            -- ^ Identifier of the potential author
  -> a                 -- ^ Submission form
  -> Enctype           -- ^ Encoding type required by the form
  -> Handler Html      -- ^ Handler
serveSubmitRelease uid form enctype = defaultLayout $ do
  let φ = handlerToWidget . runDB
  setTitle "Новая публикация"
  status <- φ (S.canSubmitAgain uid)
  case status of
    S.CanSubmit -> do
      formId   <- newIdent
      buttonId <- newIdent
      $(widgetFile "submit-release")
    S.AlreadySubmitted rid -> do
      artist  <- userName     . fromJust <$> φ (get uid)
      release <- releaseTitle . fromJust <$> φ (get rid)
      let ξ = getSlug . mkSlug
          releaseUrl = ReleaseR (ξ artist) (ξ release)
      $(widgetFile "submit-release-already")
    S.CannotSubmitYet next ->
      $(widgetFile "submit-release-next")

-- | Get current year.

getCurrentYear :: IO Int
getCurrentYear = do
  (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
  return (fromIntegral year)

-- | Generate year field given lower and upper bounds for this value.

yearField
  :: Int               -- ^ Lower bound for year (inclusive)
  -> Int               -- ^ Upper bound for year (inclusive)
  -> Field Handler Int
yearField miny maxy = check checkYear intField
  where checkYear :: Int -> Either Text Int
        checkYear year
          | year < miny = Left "Слишком давно."
          | year > maxy = Left "Машина времени?"
          | otherwise   = Right year

-- | Generate settings for year field given lower and upper bounds for this
-- value.

yearSettings
  :: Int               -- ^ Lower bound for year (inclusive)
  -> Int               -- ^ Upper bound for year (inclusive)
  -> FieldSettings App
yearSettings miny maxy =
  FieldSettings (SomeMessage ("Год" :: Text)) Nothing Nothing Nothing
    [ ("class", "form-control") -- bootstrap thing
    , ("min", sformat int miny)
    , ("max", sformat int maxy) ]

-- | Custom field to collect variable-length list of tracks, or rather
-- instructions how to create tracks in form of 'S.CreateTrack' data types.
--
-- Every field /must/ provide name of track and actual audio file in FLAC
-- format.

tracksField :: Field Handler (NonEmpty S.CreateFile)
tracksField = Field parse view Multipart
  where parse titles' files' =
          return . either (Left . SomeMessage) (Right . Just) $ do
            titles <- mapM checkTitle titles'
            files  <- mapM checkUploadedFile files'
            let toCreateFile t f =
                  S.CreateFile
                    (H.mkTitle $ T.unpack t)
                    (fileMove f . fromAbsFile)
            return . NE.fromList $ zipWith toCreateFile titles files
        view ident name attrs _ required =
          let baseId      = ident  <> "-"
              addTrackId  = baseId <> "add-track"
              remTrackId  = baseId <> "rem-track"
              trackListId = baseId <> "tracklist"
              indicies    = sformat int <$> [1..maxTrackNumber]
          in $(widgetFile "tracks-field")

-- | Check single title. TODO Add additional checks here.

checkTitle :: Text -> Either Text Text
checkTitle title =
  if T.null title
  then Left "Название не может быть пустым."
  else Right title

-- | Check single uploaded file.

checkUploadedFile :: FileInfo -> Either Text FileInfo
checkUploadedFile info =
  if fileContentType info == "audio/flac"
  then Right info
  else Left "Неверный формат, только FLAC является приемлемым форматом."
