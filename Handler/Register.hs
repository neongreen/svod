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

import Data.Char (isLower, isUpper, isDigit)
import Import
import System.IO.Unsafe (unsafePerformIO)
import Yesod.Auth.Email (saltPass)
import Yesod.Form.Bootstrap3
import qualified Crypto.Nonce as Nonce
import qualified Data.Text as T
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
  <*> areq passwdField (bfs ("Пароль" :: Text)) Nothing
  <*> areq passwdField' (bfs ("Повторите пароль" :: Text)) Nothing
  where nameField = checkM checkName textField
        checkName :: Text -> Handler (Either Text Text)
        checkName name = do
          -- TODO We must check user names so they are appropriate.
          muser <- runDB . S.getUserBySlug . mkSlug $ name
          return $ case muser of
            Nothing -> Right name
            Just  _ -> Left  "Кто-то такой уже есть…"

        emailField' = checkM checkEmail emailField
        checkEmail :: Text -> Handler (Either Text Text)
        checkEmail email = do
          muser <- runDB (S.getUserByEmail email)
          return $ case muser of
            Nothing -> Right email
            Just  _ -> Left "Этот адрес уже привязан к другому профилю."

        passwdField = check checkPasswordStrength passwordField

        passwdField' = check theSame passwordField
        theSame :: Text -> Either Text Text
        theSame = Right
          -- do
          -- result <- fst . fst <$> runFormPost registrationForm
          -- return $ case result of
          --   FormSuccess RegistrationForm {..} ->
          --     if password /= rfPass0
          --     then Left "Пароли не совпадают."
          --     else Right password
          --   _ -> Right password

-- | Serve registration page as visited for the first time.

getRegisterR :: Handler Html
getRegisterR = do
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

-- | Too weak passwords are simply not allowed. A bit radical, but…

checkPasswordStrength :: Text -> Either Text Text
checkPasswordStrength password
  | T.length password < 10 =
    Left "Этот пароль слишком короткий, нужно минимум 10 символов."
  | isNothing (T.find isLower password) =
    Left "Нужно чтобы была хотя бы одна строчная буква."
  | isNothing (T.find isUpper password) =
    Left "Нужно чтобы была хотя бы одна заглавная буква."
  | isNothing (T.find isDigit password) =
    Left "Нужно чтобы была хотя бы одна цифра в пароле."
  | otherwise = Right password

-- | Send email with link that user has to click in order to verify
-- (activate) his\/her account.

sendEmail
  :: Text              -- ^ User name
  -> Text              -- ^ Email address
  -> Text              -- ^ Verification URL
  -> Handler ()
sendEmail _ _ _ = return ()
