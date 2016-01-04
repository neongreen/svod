-- |
-- Module      :  Handler.StarRelease
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Process request to star\/unstar release.
--
-- See also: "Widget.StarRelease".

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.StarRelease
  ( postStarReleaseR )
where

import Data.Maybe (fromJust)
import Helper.Access (releaseViaSlug)
import Helper.Rendering (toInt)
import Import
import qualified Svod as S

-- | Process request to star\/unstar release and return current status as a
-- JSON value.
--
-- POST request should have @"artist-slug"@ and @"release-slug"@ parameters
-- identifying release as well as 'defaultCsrfParamName' parameter
-- containing CSRF-protection token.
--
-- Response is a JSON object with two attributes:
--     * @active@ — Boolean value telling whether the release is starred
--     by current user after executing of the action and
--     * @count@ — the total number of users who have starred the release.

postStarReleaseR :: Handler TypedContent
postStarReleaseR = do
  checkCsrfParamNamed defaultCsrfParamName
  starrer <- fromJust <$> maybeAuthId
  (aslug', rslug') <- runInputPost $ (,)
    <$> ireq textField "artist-slug"
    <*> ireq textField "release-slug"
  aslug <- parseSlug aslug'
  rslug <- parseSlug rslug'
  releaseViaSlug aslug rslug $ \_ release' -> do
    let release = entityKey release'
    active <- runDB (S.toggleStar release starrer)
    n      <- runDB (S.starCount release)
    return . toTypedContent . object $
      ["active" .= active, "count" .= toInt n]
