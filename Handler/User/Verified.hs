-- |
-- Module      :  Handler.User.Verified
-- Copyright   :  © 2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify users manually.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.User.Verified
  ( putUserVerifiedR
  , deleteUserVerifiedR )
where

import Helper.Auth
import Helper.Property (changeUserProperty)
import Import
import qualified Svod as S

-- | Very user manually.
--
-- PUT request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

putUserVerifiedR :: Slug -> Handler TypedContent
putUserVerifiedR slug = do
  checkAuthWith isStaff
  changeUserProperty (S.setVerified True) UserR slug

-- | Unverify user.
--
-- DELETE request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

deleteUserVerifiedR :: Slug -> Handler TypedContent
deleteUserVerifiedR slug = do
  checkAuthWith isStaff
  changeUserProperty (S.setVerified False) UserR slug
