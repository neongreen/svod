-- |
-- Module      :  Handler.FollowUser
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
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
import Import
import qualified Svod as S

-- | Process request to follow \/unfollow user and return current status as
-- a JSON value.
--
-- POST request should have @"slug"@ parameter identifying user to follow
-- and 'defaultCsrfParamName' parameter containing CSRF-protection token.

postFollowUserR :: Handler TypedContent
postFollowUserR = do
  checkCsrfParamNamed defaultCsrfParamName
  follower <- fromJust <$> maybeAuthId
  slug     <- runInputPost (ireq textField "slug")
  userViaSlug (mkSlug slug) $ \target' -> do
    let target = entityKey target'
    active <- runDB (S.toggleFollowing target follower)
    n'     <- runDB (S.followerCount target)
    let n = fromIntegral n' :: Int
    return . toTypedContent . object $
      ["active" .= active, "count" .= n]
