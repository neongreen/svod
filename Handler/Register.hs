-- |
-- Module      :  Handler.Register
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The registration form.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Register
  ( getRegisterR
  , processRegistration )
where

import Helper.Auth (checkUserName, checkUserEmail, checkPassStrength)
import Helper.Form
import Import
import Network.Mail.Mime hiding (htmlPart)
import System.IO.Unsafe (unsafePerformIO)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamletFile)
import Text.Shakespeare.Text (textFile)
import Yesod.Auth.Email (saltPass)
import Yesod.Form.Bootstrap3
import qualified Crypto.Nonce            as Nonce
import qualified Data.Text.Lazy.Builder  as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Svod                    as S

-- | When user wants to register on our site, here is what he\/she has to
-- submit.

data RegistrationForm = RegistrationForm
  { rfName  :: Text    -- ^ User name
  , rfEmail :: Text    -- ^ User's email address
  , rfPass0 :: Text    -- ^ Password
  , rfPass1 :: Text    -- ^ Password repeated
  }

-- | Registration form to use on pages.

registrationForm :: Form RegistrationForm
registrationForm = renderBootstrap3 BootstrapBasicForm $ RegistrationForm
  <$> areq nameField (μ' "name" "Имя") Nothing
  <*> areq emailField' (μ "email" "Почта") Nothing
  <*> areq passField (μ "password" "Пароль") Nothing
  <*> areq passField (μ "password_again" "Повторите пароль") Nothing
  where nameField   = checkM (checkUserName False) textField
        emailField' = checkM checkUserEmail emailField
        passField   = check checkPassStrength passwordField

-- | Serve registration page as visited for the first time.

getRegisterR :: Handler Html
getRegisterR = do
  already <- isJust <$> maybeAuthId
  when already (redirect HomeR)
  (form, enctype) <- generateFormPost registrationForm
  serveRegistration form enctype

-- | Process registration request.

processRegistration :: Handler TypedContent
processRegistration = do
  ((result, form), enctype) <- runFormPost registrationForm
  case result of
    FormSuccess RegistrationForm {..} ->
      if rfPass0 == rfPass1
      then do
        verkey   <- liftIO randomKey
        password <- liftIO (saltPass rfPass0)
        user     <- runDB (S.addUnverified rfName rfEmail password verkey)
        let uid       = entityKey user
            User {..} = entityVal user
#if DEVELOPMENT
        runDB (S.setVerified True uid)
        setMsg MsgSuccess "Профиль автоматически активирован."
#else
        urlRender <- getUrlRender
        sendEmail userName userEmail (urlRender $ VerifyR userVerkey)
        setMsg MsgInfo [shamlet|
Мы выслали вам ссылку для подтверждения регистрации на #
<strong>
  #{userEmail}
.
|]
#endif
        when (unSlug userSlug == "свод") $ runDB $ do
          S.setVerified True uid
          S.setUserStatus AdminUser uid
        toTypedContent <$> setCredsRedirect Creds
          { credsPlugin = "custom"
          , credsIdent  = unSlug userSlug
          , credsExtra  = [] }
      else do
        setMsg MsgDanger "Пароли не совпадают."
        toTypedContent <$> serveRegistration form enctype
    _ -> toTypedContent <$> serveRegistration form enctype

-- | Serve registration page.

serveRegistration :: ToWidget App a
  => a                 -- ^ Registration form
  -> Enctype           -- ^ Encoding type required by the form
  -> Handler Html      -- ^ Handler
serveRegistration form enctype = defaultLayout $ do
  -- TODO Users must fill out CAPTCHA on every attempt.
  setTitle "Регистрация"
  $(widgetFile "register")

-- | Generate a random alphanumeric string.

randomKey :: IO Text
randomKey = Nonce.nonce128urlT defaultNonceGen

-- | Taken from "Yesod.Auth.Email".

defaultNonceGen :: Nonce.Generator
defaultNonceGen = unsafePerformIO Nonce.new
{-# NOINLINE defaultNonceGen #-}

-- | Send email with link that user has to click in order to verify
-- (activate) his\/her account.
--
-- This uses standard system utility @sendmail@. It's good enough for now.

sendEmail
  :: Text              -- ^ User name
  -> Text              -- ^ Email address
  -> Text              -- ^ Verification URL
  -> Handler ()
sendEmail name email url = do
  render <- getUrlRender
  let textPart = Part
        { partType     = "text/plain; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent  = T.encodeUtf8 . T.toLazyText $
           $(textFile "templates/verification-email.txt") render
        , partHeaders  = [] }
      htmlPart = Part
        { partType     = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent  = renderHtml
           $(shamletFile "templates/verification-email.hamlet")
        , partHeaders  = [] }
  liftIO $ renderSendMail
    (emptyMail $ Address (Just "Проект «Свод»") "noreply")
    { mailTo      = [Address Nothing email]
    , mailHeaders = [("Subject", "Подтверждение регистрации")]
    , mailParts   = [[textPart, htmlPart]] }
