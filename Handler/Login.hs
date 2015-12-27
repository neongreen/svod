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

import Helper.Auth (checkUserName, checkPassCorrect)
import Import
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
  <*> areq passwordField (bfs ("Пароль" :: Text)) Nothing
  where nameField = checkM (checkUserName True) textField

-- | Serve a page with login form.

getLoginR :: Handler Html
getLoginR = do
  already <- isJust <$> maybeAuthId
  when already (redirect HomeR)
  (form, enctype) <- generateFormPost loginForm
  serveLogin form enctype

-- | Process submitted login form.

postLoginR :: Handler TypedContent
postLoginR = do
  ((result, form), enctype) <- runFormPost loginForm
  case result of
    FormSuccess LoginForm {..} -> do
      muser <- mkSlug lfName >>= runDB . S.getUserBySlug
      acres <- runDB (checkPassCorrect muser lfPassword)
      case entityVal <$> acres of
        Left msg -> do
          setMsg MsgDanger (toHtml msg)
          toTypedContent <$> serveLogin form enctype
        Right user ->
          toTypedContent <$> setCredsRedirect Creds
            { credsPlugin = "custom"
            , credsIdent  = unSlug (userSlug user)
            , credsExtra  = [] }
    _ -> toTypedContent <$> serveLogin form enctype

-- | Serve login page.

serveLogin :: ToWidget App a
  => a                 -- ^ Login form
  -> Enctype           -- ^ Encoding type required by the form
  -> Handler Html      -- ^ Handler
serveLogin form enctype = defaultLayout $ do
  -- TODO User must fill out CAPTCHA on every attempt.
  setTitle "Вход"
  $(widgetFile "login")
