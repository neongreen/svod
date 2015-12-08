-- |
-- Module      :  Handler.Login
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Login handler.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Login
  ( getLoginR
  , postLoginR )
where

import Data.Maybe (fromJust)
import Import
import Yesod.Auth.Email (isValidPass)
import Yesod.Form.Bootstrap3
import qualified Svod as S

-- | What do we need to login a user?

data LoginForm = LoginForm
  { lfName     :: Text -- ^ User name
  , lfPassword :: Text -- ^ Password
  }

-- | Login form to use on pages.

loginForm :: Form LoginForm
loginForm = renderBootstrap3 BootstrapBasicForm $ LoginForm
  <$> areq nameField (withAutofocus $ bfs ("Имя" :: Text)) Nothing
  <*> areq passwdField (bfs ("Пароль" :: Text)) Nothing
  where nameField = checkM checkName textField
        checkName :: Text -> Handler (Either Text Text)
        checkName name = do
          muser <- runDB $ S.getUserBySlug (mkSlug name)
          case muser of
            Nothing -> return (Left "Нет такого пользователя.")
            Just  _ -> setPocket name >> return (Right name)
        passwdField = checkM passwordMatches passwordField

-- | Serve a page with login form.

getLoginR :: Handler Html
getLoginR = do
  (form, enctype) <- generateFormPost loginForm
  serveLogin form enctype

-- | Process submitted login form.

postLoginR :: Handler TypedContent
postLoginR = do
  ((result, form), enctype) <- runFormPost loginForm
  case result of
    FormSuccess LoginForm {..} -> do
      muser <- runDB . S.getUserBySlug . mkSlug $ lfName
      case muser of
        Nothing -> toTypedContent <$>
          setMsg MsgDanger "Нет такого пользователя."
        Just user -> toTypedContent <$> setCredsRedirect Creds
          { credsPlugin = "custom"
          , credsIdent  = getSlug . userSlug . entityVal $ user
          , credsExtra  = [] }
    _ -> toTypedContent <$> serveLogin form enctype

-- | Serve login page.

serveLogin :: ToWidget App a
  => a                 -- ^ Login form
  -> Enctype           -- ^ Encoding type required by the form
  -> Handler Html      -- ^ Handler
serveLogin form enctype = do
  already <- isJust <$> maybeAuthId
  when already (redirect HomeR)
  defaultLayout $ do
    -- TODO User must fill out CAPTCHA on every attempt.
    setTitle "Вход"
    $(widgetFile "login")

-- | Check if given password is correct.

passwordMatches :: Text -> Handler (Either Text Text)
passwordMatches given = do
  let msg = Left "Этот пароль не подходит."
  mname <- getPocket
  case mname of
    Nothing -> return msg
    Just name -> do
      muid <- runDB $ S.getUserBySlug (mkSlug name)
      case entityKey <$> muid of
        Nothing -> return msg
        Just uid -> do
          salted <- fromJust <$> runDB (S.getPassword uid)
          return $ if isValidPass given salted
            then Right given
            else msg
