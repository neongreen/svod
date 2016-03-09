-- |
-- Module      :  Handler.User.Admin
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
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

import Helper.Property (changeUserProperty)
import Import
import qualified Svod as S

-- | Make specific user an admin.
--
-- PUT request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

putUserAdminR :: Slug -> Handler TypedContent
putUserAdminR = changeUserProperty (S.setUserStatus AdminUser) UserR

-- | Remove admin privileges of a specific user.
--
-- DELETE request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

deleteUserAdminR :: Slug -> Handler TypedContent
deleteUserAdminR = changeUserProperty (S.setUserStatus NormalUser) UserR
