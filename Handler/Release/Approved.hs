-- |
-- Module      :  Handler.Release.Approved
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Approving of releases.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Release.Approved
  ( putReleaseApprovedR )
where

import Helper.Auth
import Helper.Property (changeReleaseProperty)
import Import
import qualified Svod as S

-- | Approve submitted release. Only admins can do that.
--
-- PUT request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token to succeed.

putReleaseApprovedR :: Slug -> Slug -> Handler TypedContent
putReleaseApprovedR uslug rslug = do
  checkAuthWith isAdmin
  changeReleaseProperty S.approveRelease ReleaseR uslug rslug
