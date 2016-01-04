-- |
-- Module      :  Handler.ChangePassword
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The procedure of password changing.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.ChangePassword
  ( getChangePasswordR
  , postChangePasswordR )
where

import Data.Maybe (fromJust)
import Helper.Auth (checkPassCorrect, checkPassStrength)
import Import
import Yesod.Auth.Email (saltPass)
import Yesod.Form.Bootstrap3
import qualified Svod as S

-- | Information user need to provide in order to change his\/her password.

data ChangePasswordForm = ChangePasswordForm
  { cpOldPass  :: Text -- ^ Old password
  , cpNewPass0 :: Text -- ^ New password
  , cpNewPass1 :: Text -- ^ New password repeated
  }

-- | Form for password changing.

changePasswordForm :: Form ChangePasswordForm
changePasswordForm = renderBootstrap3 BootstrapBasicForm $ ChangePasswordForm
  <$> areq oldPassField (withAutofocus $ bfs ("Текущий пароль" :: Text)) Nothing
  <*> areq newPassField (bfs ("Новый пароль" :: Text)) Nothing
  <*> areq newPassField (bfs ("Повторите новый пароль" :: Text)) Nothing
  where oldPassField  = checkM passwordMatch' passwordField
        newPassField  = check checkPassStrength passwordField

-- | Serve page with form allowing to change user' password.

getChangePasswordR :: Handler Html
getChangePasswordR = do
  (form, enctype) <- generateFormPost changePasswordForm
  serveChangePassword form enctype

-- | Process results of password change form.

postChangePasswordR :: Handler Html
postChangePasswordR = do
  ((result, form), enctype) <- runFormPost changePasswordForm
  case result of
    FormSuccess ChangePasswordForm {..} ->
      if cpNewPass0 == cpNewPass1
      then do
        uid      <- fromJust <$> maybeAuthId
        password <- liftIO (saltPass cpNewPass0)
        runDB (S.setPassword uid password)
        setMsg MsgSuccess "Пароль изменён успешно."
      else setMsg MsgDanger "Пароли не совпадают."
    _ -> return ()
  serveChangePassword form enctype

-- | Serve page with “change password” form.

serveChangePassword :: ToWidget App a
  => a                 -- ^ “Change password” form
  -> Enctype           -- ^ Encoding type required by the form
  -> Handler Html      -- ^ Handler
serveChangePassword form enctype = defaultLayout $ do
  setTitle "Изменение пароля"
  $(widgetFile "change-password")

-- | Check if given password is correct.

passwordMatch' :: Text -> Handler (Either Text Text)
passwordMatch' given = do
  muser <- maybeAuth
  acres <- runDB (checkPassCorrect muser given)
  return (acres >> Right given)
