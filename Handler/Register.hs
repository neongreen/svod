-- |
-- Module      :  Handler.Register
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The registration form.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Register
  ( getRegisterR
  , postRegisterR )
where

import Helper.Auth
  ( checkUserName
  , checkUserEmail
  , checkPassStrength
  , checkPassConfirm )
import Import
import System.IO.Unsafe (unsafePerformIO)
import Yesod.Auth.Email (saltPass)
import Yesod.Form.Bootstrap3
import qualified Crypto.Nonce as Nonce
import qualified Svod as S

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
  <$> areq nameField (withAutofocus $ bfs ("Имя" :: Text)) Nothing
  <*> areq emailField' (bfs ("Почта" :: Text)) Nothing
  <*> areq passField (bfs ("Пароль" :: Text)) Nothing
  <*> areq passField' (bfs ("Повторите пароль" :: Text)) Nothing
  where nameField   = checkM (checkUserName False) textField
        emailField' = checkM checkUserEmail emailField
        passField   = checkM checkPassStrength passwordField
        passField'  = checkM checkPassConfirm passwordField

-- | Serve registration page as visited for the first time.

getRegisterR :: Handler Html
getRegisterR = do
  already <- isJust <$> maybeAuthId
  when already (redirect HomeR)
  (form, enctype) <- generateFormPost registrationForm
  serveRegistration form enctype

-- | Process registration request.

postRegisterR :: Handler TypedContent
postRegisterR = do
  ((result, form), enctype) <- runFormPost registrationForm
  case result of
    FormSuccess RegistrationForm {..} -> do
      verkey   <- liftIO randomKey
      password <- liftIO (saltPass rfPass0)
      user     <- runDB (S.addUnverified rfName rfEmail password verkey)
      let uid       = entityKey user
          User {..} = entityVal user
#if DEVELOPMENT
      runDB (S.setVerified uid)
      setMsg MsgSuccess "Профиль автоматически активирован."
#else
      urlRender <- getUrlRender
      sendEmail userName userEmail (urlRender $ VerifyR userVerkey)
      setMsg MsgInfo $
        "Мы выслали вам ссылку для подтверждения регистрации на `" <>
        userEmail <> "`."
#endif
      when (userSlug == mkSlug "Свод") $ runDB $ do
        S.setVerified uid
        S.setStaff uid True
        S.setAdmin uid True
      toTypedContent <$> setCredsRedirect Creds
        { credsPlugin = "custom"
        , credsIdent  = getSlug userSlug
        , credsExtra  = [] }
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

sendEmail
  :: Text              -- ^ User name
  -> Text              -- ^ Email address
  -> Text              -- ^ Verification URL
  -> Handler ()
sendEmail _ _ _ = return () -- TODO Send real emails
