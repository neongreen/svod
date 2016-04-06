-- |
-- Module      :  Handler.Release.Edit
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Display and edit parameters of submitted release. Admins can edit even
-- already published works.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Release.Edit
  ( getReleaseEditR
  , processReleaseChange )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust)
import Helper.Access (releaseViaSlug)
import Helper.Auth
import Helper.Form
import Helper.Json (releaseJson)
import Helper.Path (getFConfig)
import Import
import Path
import Yesod.Core.Types (FileInfo (..))
import Yesod.Form.Bootstrap3
import qualified Sound.HTagLib as H
import qualified Svod          as S

-- | Data that should be provided in order to edit a release.

data EditReleaseForm = EditReleaseForm
  { erLicense :: License    -- ^ License of the work
  , erGenre   :: Maybe Text -- ^ Genre (if any)
  , erYear    :: Int        -- ^ Year of publication
  , erTracks  :: NonEmpty (Text, FileInfo) -- ^ Tracks to create
  , erDemo    :: Bool       -- ^ Is this a demo?
  , erDesc    :: Textarea   -- ^ Description of the release
  }

-- | Form for release submission.

editReleaseForm :: Release -> NonEmpty Track -> Form EditReleaseForm
editReleaseForm Release {..} tracks html = do
  maxy <- liftIO getCurrentYear
  let miny = maxy - 3
      form = EditReleaseForm
        <$> areq licenseField (μ "license" "Лицензия") (Just releaseLicense)
        <*> aopt textField (μ "genre" "Жанр") (Just releaseGenre)
        <*> areq (yearField miny maxy) (μL "year" "Год" miny maxy)
          (Just $ fromIntegral releaseYear)
        <*> areq tracksField (μ "track" "Список записей")
          (Just $ g <$> tracks)
        <*> areq checkBoxField (μ "demo" "Демо") (Just releaseDemo)
        <*> areq textareaField (μ "description" "Описание")
          (Just . Textarea . unDescription $ releaseDescription)
      g :: Track -> (Text, FileInfo)
      g Track {..} = (trackTitle, FileInfo "" "audio/flac"
        (yield "foo") (const $ return ()))
  renderBootstrap3 BootstrapBasicForm form html

-- | Render form for release editing.

getReleaseEditR
  :: Slug              -- ^ User slug
  -> Slug              -- ^ Release slug
  -> Handler Html
getReleaseEditR uslug rslug = releaseViaSlug uslug rslug $ \_ release -> do
  let isFinalized = isJust . releaseFinalized . entityVal $ release
  checkAuthWith (isAdmin <> if isFinalized then mempty else isSelf uslug)
  tracks <- runDB . S.getReleaseTracklist . entityKey $ release
  (form, enctype) <-
    generateFormPost (editReleaseForm (entityVal release) tracks)
  serveEditRelease uslug rslug form enctype

-- | Process release editing request.

processReleaseChange
  :: Slug              -- ^ User slug
  -> Slug              -- ^ Release slug
  -> Handler TypedContent
processReleaseChange uslug rslug = releaseViaSlug uslug rslug $ \u release -> do
  let (Entity rid r') = release
      isFinalized = isJust (releaseFinalized r')
  checkAuthWith (isAdmin <> if isFinalized then mempty else isSelf uslug)
  tracks <- runDB (S.getReleaseTracklist rid)
  ((result, form), enctype) <- runFormPost (editReleaseForm r' tracks)
  case result of
    FormSuccess EditReleaseForm {..} -> do
      let toCreateFile (t, f) =
            S.CreateFile (H.mkTitle t) (fileMove f . fromAbsFile)
          rm = S.ReleaseMeta
            { rmLicense = erLicense
            , rmGenre  = H.mkGenre <$> erGenre
            , rmYear   = fromJust (H.mkYear erYear)
            , rmTracks = toCreateFile <$> erTracks
            , rmDemo   = erDemo
            , rmDesc   = mkDescription (unTextarea erDesc)
            , rmArtist = entityKey u
            , rmAlbum  = H.mkAlbum (releaseTitle r') }
      fconfig <- getFConfig
      outcome <- runDB (S.editRelease fconfig rid rm)
      case outcome of
        Left msg ->
          selectRep $ do
            -- HTML representation
            provideRep $ do
              setMsg MsgDanger (toHtml msg)
              serveEditRelease uslug rslug form enctype
            -- JSON representation
            provideRep . return . object $ ["failed" .= msg]
        Right () -> do
          r@Release {..} <- fromJust <$> runDB (get rid)
          render <- getUrlRender
          selectRep $ do
            -- HTML representation
            provideRep $ do
              setMsg MsgSuccess [shamlet|
Публикация #
<a href="#{render $ ReleaseR uslug rslug}">
  #{releaseTitle}
изменена успешно.
|]
              redirect (ReleaseR uslug rslug) :: Handler Html
            -- JSON representation
            provideRep $ do
              stars <- runDB (S.starCount rid)
              return (releaseJson render stars (entityVal u) r)

    _ ->
      selectRep $ do
        -- HTML representation
        provideRep (serveEditRelease uslug rslug form enctype)
        -- JSON representation
        provideRep . return . object $
          ["failed" .= ("form parsing failed" :: Text)]

-- | Serve the “edit release” page.

serveEditRelease :: ToWidget App a
  => Slug              -- ^ Artist slug
  -> Slug              -- ^ Release slug
  -> a                 -- ^ Release editing form
  -> Enctype           -- ^ Encoding type required by the form
  -> Handler Html      -- ^ Handler
serveEditRelease uslug rslug form enctype = defaultLayout $ do
  setTitle "Редактировать публикацию"
  formId   <- newIdent
  buttonId <- newIdent
  $(widgetFile "edit-release")
