-- |
-- Module      :  Handler.User.Banned
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Ban and unban users.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.User.Banned
  ( putUserBannedR
  , deleteUserBannedR )
where

import Helper.Property (changeUserProperty)
import Import
import qualified Svod as S

-- | Ban a user.
--
-- PUT request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

putUserBannedR :: Slug -> Handler TypedContent
putUserBannedR = changeUserProperty (S.setBanned True) UserR

-- | Unban a user.
--
-- DELETE request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

deleteUserBannedR :: Slug -> Handler TypedContent
deleteUserBannedR = changeUserProperty (S.setBanned False) UserR
