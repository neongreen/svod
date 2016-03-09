-- |
-- Module      :  Handler.Release.Starred
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Process request to star\/unstar release.
--
-- See also: "Widget.StarRelease".

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Release.Starred
  ( putReleaseStarredR
  , deleteReleaseStarredR )
where

import Helper.Access (userViaSlug, releaseViaSlug)
import Helper.Rendering (toInt)
import Import
import qualified Svod as S

-- | Process request to star a release and return current status as a JSON
-- value.
--
-- PUT request must have 'defaultCsrfParamName' parameter containing
-- CSRF-protection token.
--
-- Response is a JSON object with two attributes:
--     * @active@ — Boolean value telling whether the release is starred
--     by specified user after the action and
--     * @count@ — the total number of users who have starred the release.

putReleaseStarredR :: Slug -> Slug -> Slug -> Handler TypedContent
putReleaseStarredR = starGeneric (S.setStar True)

-- | Process request to unstar a release and return current status as a JSON
-- value. See 'putReleaseStarredR'.

deleteReleaseStarredR :: Slug -> Slug -> Slug -> Handler TypedContent
deleteReleaseStarredR = starGeneric (S.setStar False)

starGeneric
  :: (ReleaseId -> UserId -> YesodDB App ()) -- ^ Database action to perform
  -> Slug              -- ^ Artist, author of release
  -> Slug              -- ^ Release slug
  -> Slug              -- ^ Slug of user who stars\/unstars
  -> Handler TypedContent
starGeneric action aslug rslug uslug = do
  checkCsrfParamNamed defaultCsrfParamName
  releaseViaSlug aslug rslug $ \_ release' ->
    userViaSlug uslug $ \starrer' -> do
      let release = entityKey release'
          starrer = entityKey starrer'
      runDB (action release starrer)
      active <- runDB (S.isStarredBy release starrer)
      n      <- runDB (S.starCount release)
      return . toTypedContent . object $
        ["active" .= active, "count" .= toInt n]
