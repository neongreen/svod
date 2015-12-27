-- |
-- Module      :  Handler.FollowUser
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Process request to follow\/unfollow user.
--
-- See also: "Widget.FollowUser".

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.FollowUser
  ( postFollowUserR )
where

import Data.Maybe (fromJust)
import Helper.Access (userViaSlug)
import Helper.Rendering (toInt)
import Import
import qualified Svod as S

-- | Process request to follow \/unfollow user and return current status as
-- a JSON value.
--
-- POST request should have @"slug"@ parameter identifying user to follow
-- and 'defaultCsrfParamName' parameter containing CSRF-protection token.
--
-- Response is a JSON object with two attributes:
--     * @active@ — Boolean value telling whether current user is
--     following target user after executing of the action and
--     * @count@ — the total number of users following target user now.

postFollowUserR :: Handler TypedContent
postFollowUserR = do
  checkCsrfParamNamed defaultCsrfParamName
  follower <- fromJust <$> maybeAuthId
  slug     <- runInputPost (ireq textField "slug") >>= parseSlug
  userViaSlug slug $ \target' -> do
    let target = entityKey target'
    active <- runDB (S.toggleFollowing target follower)
    n      <- runDB (S.followerCount target)
    return . toTypedContent . object $
      ["active" .= active, "count" .= toInt n]
