-- |
-- Module      :  Handler.User.Verified
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
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

import Helper.Property (changeUserProperty)
import Import
import qualified Svod as S

-- | Very user manually.
--
-- PUT request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

putUserVerifiedR :: Slug -> Handler TypedContent
putUserVerifiedR = changeUserProperty (S.setVerified True) UserR

-- | Unverify user.
--
-- DELETE request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

deleteUserVerifiedR :: Slug -> Handler TypedContent
deleteUserVerifiedR = changeUserProperty (S.setVerified False) UserR
