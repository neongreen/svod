-- |
-- Module      :  Handler.User.Followers
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Serve list of followers of a given user.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.User.Followers
  ( getUserFollowersR )
where

import Import

getUserFollowersR :: Slug -> Handler Html
getUserFollowersR = undefined -- TODO Also is JSON
