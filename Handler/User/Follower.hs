-- |
-- Module      :  Handler.User.Follower
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Follow and unfollow users.

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.User.Follower
  ( putUserFollowerR
  , deleteUserFollowerR )
where

import Helper.Access (userViaSlug)
import Helper.Rendering (toInt)
import Import
import qualified Svod as S

-- | Process request to follow a user and return current status as JSON
-- value.
--
-- PUT request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token.
--
-- Response is a JSON object with two attributes:
--     * @active@ — Boolean value telling whether specified user is
--     following target user after the action and
--     * @count@ — the total number of users following target user now.

putUserFollowerR :: Slug -> Slug -> Handler TypedContent
putUserFollowerR = followGeneric (S.setFollowing True)

-- | Process request to unfollow a user and return current status as JSON
-- value. See 'putUserFollowerR' for more information.

deleteUserFollowerR :: Slug -> Slug -> Handler TypedContent
deleteUserFollowerR = followGeneric (S.setFollowing False)

followGeneric
  :: (UserId -> UserId -> YesodDB App ()) -- ^ Database action to perform
  -> Slug              -- ^ Whom to follow?
  -> Slug              -- ^ Follower
  -> Handler TypedContent
followGeneric action targetSlug followerSlug = do
  checkCsrfParamNamed defaultCsrfParamName
  userViaSlug targetSlug $ \target' ->
    userViaSlug followerSlug $ \follower' -> do
      let target   = entityKey target'
          follower = entityKey follower'
      runDB (action target follower)
      active <- runDB (S.isFollower target follower)
      n      <- runDB (S.followerCount target)
      return . toTypedContent . object $
        ["active" .= active, "count" .= toInt n]
