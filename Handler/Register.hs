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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Register
  ( getRegisterR
  , processRegistration )
where

import Data.Maybe (fromJust)
import Helper.Auth
import Helper.Email (startEmailVerificationCycle)
import Helper.Form
import Import
import Yesod.Auth.Email (saltPass)
import Yesod.Form.Bootstrap3
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
  <$> areq nameField (μ' "name" "Имя") Nothing
  <*> areq emailField' (μ "email" "Почта") Nothing
  <*> areq passField (μ "password" "Пароль") Nothing
  <*> areq passField (μ "password_again" "Повторите пароль") Nothing
  where nameField   = checkM (checkUserName False) textField
        emailField' = check checkEmailAddress emailField
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
        password <- liftIO (saltPass rfPass0)
        let email = fromJust (mkEmail rfEmail)
        user     <- runDB (S.addUnverified rfName email password "")
        let uid       = entityKey user
            User {..} = entityVal user
        startEmailVerificationCycle email userName uid
        when (unSlug userSlug == "свод") . runDB $ do
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
  -- TODO CAPTCHA Users must fill out CAPTCHA on every attempt.
  setTitle "Регистрация"
  $(widgetFile "register")
