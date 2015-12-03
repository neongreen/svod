-- |
-- Module      :  Handler.DeleteUser
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Delete users. Only admins can delete users. It shouldn't be used because
-- banning is more efficient way to handle users anyway and nothing will
-- prevent misbehaving users from registering on the site again.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.DeleteUser
  ( postDeleteUserR )
where

import Import

-- | Process request to delete a user.

postDeleteUserR :: Handler Html
postDeleteUserR = undefined
