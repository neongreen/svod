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
import Helper.Auth
import Helper.Form
import Helper.Json (releaseJson)
import Helper.Path (getFConfig)
import Helper.Rendering (toJSONId)
import Import
import Path
import Yesod.Form.Bootstrap3
import qualified Sound.HTagLib as H
import qualified Svod          as S

-- | Information user must submit to upload something.

data SubmitReleaseForm = SubmitReleaseForm
  { srTitle   :: Text                  -- ^ Title of entire work (album)
  , srLicense :: License               -- ^ License of the work
  , srGenre   :: Maybe Text            -- ^ Genre (if any)
  , srYear    :: Int                   -- ^ Year of publication
  , srTracks  :: NonEmpty (Text, FileInfo) -- ^ Tracks to create
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
        <*> areq licenseField (μ "license" "Лицензия") Nothing
        <*> aopt textField (μ "genre" "Жанр") Nothing
        <*> areq (yearField miny maxy) (μL "year" "Год" miny maxy) (Just maxy)
        <*> areq tracksField (μ "track" "Список записей") Nothing
        <*> areq checkBoxField (μ "demo" "Демо") (Just False)
        <*> areq textareaField (μ "description" "Описание") Nothing
  renderBootstrap3 BootstrapBasicForm form html

-- | Serve page with “submit release” form.

getSubmitReleaseR :: Handler Html
getSubmitReleaseR = do
  checkAuthWith isVerified
  (form, enctype) <- generateFormPost submitReleaseForm
  uid <- fromJust <$> maybeAuthId
  serveSubmitRelease uid form enctype

-- | Process submission.

processReleaseSubmission :: Slug -> Handler TypedContent
processReleaseSubmission slug = do
  user <- fromJust <$> maybeAuth
  let u@User {..} = entityVal user
      uid         = entityKey user
  checkAuthWith isVerified
  status <- runDB (S.canSubmitAgain uid)
  unless (status == S.CanSubmit && slug == userSlug) $
    permissionDenied "Вы не можете опубликовать что-либо сейчас."
  ((result, form), enctype) <- runFormPost submitReleaseForm
  case result of
    FormSuccess SubmitReleaseForm {..} -> do
      let toCreateFile (t, f) =
            S.CreateFile (H.mkTitle t) (fileMove f . fromAbsFile)
          rm = S.ReleaseMeta
            { rmArtist  = uid
            , rmAlbum   = H.mkAlbum srTitle
            , rmGenre   = H.mkGenre <$> srGenre
            , rmYear    = fromJust (H.mkYear srYear)
            , rmDesc    = mkDescription (unTextarea srDesc)
            , rmLicense = srLicense
            , rmDemo    = srDemo
            , rmTracks  = toCreateFile <$> srTracks }
      fconfig <- getFConfig
      outcome <- runDB (S.submitRelease fconfig rm)
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
опубликована. В период рассмотрения администрация может посылать вам электронные
письма чтобы обсудить или поправить что-либо, пожалуйста реагируйте
своевременно.
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
