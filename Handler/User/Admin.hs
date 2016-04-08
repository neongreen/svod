-- |
-- Module      :  Handler.User.Admin
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module allows to turn a user into admin and back.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.User.Admin
  ( putUserAdminR
  , deleteUserAdminR )
where

import Helper.Auth
import Helper.Property (changeUserProperty)
import Import
import qualified Svod as S

-- | Make specific user an admin.
--
-- PUT request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

putUserAdminR :: Slug -> Handler TypedContent
putUserAdminR slug = do
  checkAuthWith isAdmin
  changeUserProperty (S.setUserStatus AdminUser) UserR slug

-- | Remove admin privileges of a specific user.
--
-- DELETE request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

deleteUserAdminR :: Slug -> Handler TypedContent
deleteUserAdminR slug = do
  checkAuthWith isAdmin
  changeUserProperty (S.setUserStatus NormalUser) UserR slug
