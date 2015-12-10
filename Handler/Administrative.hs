-- |
-- Module      :  Handler.Administrative
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Administrative actions on users. All of these can be performed only by
-- staff or admins. All of these are usually require confirmation on UI
-- level, are CSRF-protected forms that are executed via POST requests. Some
-- of these can have serious repercussions, for example deletion of user can
-- wipe his releases.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Administrative
  ( postVerifyUserR
  , postBanUserR
  , postDeleteUserR
  , postMakeStaffR
  , postMakeAdminR )
where

import Import

-- | Verify a user, this makes following link from email unnecessary for
-- that user.

postVerifyUserR :: Handler Html
postVerifyUserR = undefined

-- | Ban a user. We use hellban, it's good for trolls.

postBanUserR :: Handler Html
postBanUserR = undefined

-- | Delete a user, use with great care.

postDeleteUserR :: Handler Html
postDeleteUserR = undefined

-- | Make a user staff member.

postMakeStaffR :: Handler Html
postMakeStaffR = undefined

-- | Make a user admin, use with great care.

postMakeAdminR :: Handler Html
postMakeAdminR = undefined
