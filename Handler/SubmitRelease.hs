-- |
-- Module      :  Handler.SubmitRelease
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
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
  , processReleaseSubmission )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust)
import Formatting (sformat, int)
import Helper.Form
import Helper.Json (releaseJson)
import Helper.Path (getFConfig)
import Helper.Rendering (toInt, toJSONId)
import Import
import Path
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
        <$> areq textField (μ' "title" "Название") Nothing
        <*> areq (selectFieldList licenses) (μ "license" "Лицензия") Nothing
        <*> aopt textField (μ "genre" "Жанр") Nothing
        <*> areq (yearField miny maxy) (μL "year" "Год" miny maxy) (Just maxy)
        <*> areq tracksField (μ "track" "Список записей") Nothing
        <*> areq checkBoxField (μ "demo" "Демо") (Just False)
        <*> areq textareaField (μ "description" "Описание") Nothing
  renderBootstrap3 BootstrapBasicForm form html
  where licenses = (licensePretty &&& id) <$> [minBound..maxBound]

-- | Serve page with “submit release” form.

getSubmitReleaseR :: Handler Html
getSubmitReleaseR = do
  (form, enctype) <- generateFormPost submitReleaseForm
  uid <- fromJust <$> maybeAuthId
  serveSubmitRelease uid form enctype

-- | Process submission.

processReleaseSubmission :: Slug -> Handler TypedContent
processReleaseSubmission slug = do
  user <- fromJust <$> maybeAuth
  let u@User {..} = entityVal user
      uid         = entityKey user
  status <- runDB (S.canSubmitAgain uid)
  unless (status == S.CanSubmit && slug == userSlug) $
    permissionDenied "Вы не можете опубликовать что-либо сейчас."
  ((result, form), enctype) <- runFormPost submitReleaseForm
  case result of
    FormSuccess SubmitReleaseForm {..} -> do
      let rm = S.ReleaseMeta
            { rmArtist  = uid
            , rmAlbum   = H.mkAlbum srTitle
            , rmGenre   = H.mkGenre <$> srGenre
            , rmYear    = fromJust (H.mkYear srYear)
            , rmDesc    = mkDescription (unTextarea srDesc)
            , rmLicense = srLicense
            , rmTracks  = srTracks }
      fconfig <- getFConfig
      outcome <- runDB (S.submitRelease fconfig rm srDemo)
      case outcome of
        Left msg ->
          selectRep $ do
            -- HTML representation
            provideRep $ do
              setMsg MsgDanger (toHtml msg)
              serveSubmitRelease uid form enctype
            -- JSON representation
            provideRep . return . object $ ["failed" .= msg]
        Right rid -> do
          r@Release {..} <- fromJust <$> runDB (get rid)
          selectRep $ do
            -- HTML representation
            provideRep $ do
              setMsg MsgSuccess [shamlet|
Публикация #
<strong>
  #{srTitle}
\ теперь ожидает рассмотрения. Пожалуйста будьте терпеливы, вы будете уведомлены
о принятом решении. Вы можете редактировать вашу работу пока она не
опубликована. В период рассмотрения администрация может посылать вам сообщения,
чтобы обсудить или поправить что-либо, пожалуйста реагируйте своевременно.
|]
              redirect (ReleaseR userSlug releaseSlug) :: Handler Html
            -- JSON representation
            provideRep $ do
              render <- getUrlRender
              stars  <- runDB (S.starCount rid)
              return (releaseJson render stars u r)

    _ ->
      selectRep $ do
        -- HTML representation
        provideRep (serveSubmitRelease uid form enctype)
        -- JSON representation
        provideRep . return . object $
          ["failed" .= ("form parsing failed" :: Text)]

-- | Serve “submit release” page.

serveSubmitRelease :: ToWidget App a
  => UserId            -- ^ Identifier of the potential author
  -> a                 -- ^ Submission form
  -> Enctype           -- ^ Encoding type required by the form
  -> Handler Html      -- ^ Handler
serveSubmitRelease uid form enctype = defaultLayout $ do
  setTitle "Новая публикация"
  status <- φ (S.canSubmitAgain uid)
  User {..} <- fromJust <$> φ (get uid)
  case status of
    S.CanSubmit -> do
      formId   <- newIdent
      buttonId <- newIdent
      $(widgetFile "submit-release")
    S.AlreadySubmitted rid -> do
      Release {..} <- fromJust <$> φ (get rid)
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
                  S.CreateFile (H.mkTitle t) (fileMove f . fromAbsFile)
            return . NE.fromList $ zipWith toCreateFile titles files
        view ident name attrs _ required =
          let baseId      = ident  <> "-"
              addTrackId  = baseId <> "add-track"
              remTrackId  = baseId <> "rem-track"
              trackListId = baseId <> "tracklist"
              indicies    = sformat int <$> [1..S.maxTrackNumber]
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
