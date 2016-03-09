-- |
-- Module      :  Handler.User.Staff
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Make users staff members and exclude them from staff.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.User.Staff
  ( putUserStaffR
  , deleteUserStaffR )
where

import Helper.Property (changeUserProperty)
import Import
import qualified Svod as S

-- | Make user staff member.
--
-- PUT request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

putUserStaffR :: Slug -> Handler TypedContent
putUserStaffR = changeUserProperty (S.setUserStatus StaffUser) UserR

-- | Make user normal user again.
--
-- DELETE request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token in order to succeed.

deleteUserStaffR :: Slug -> Handler TypedContent
deleteUserStaffR = changeUserProperty (S.setUserStatus NormalUser) UserR
