-- |
-- Module      :  Handler.StarRelease
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
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
import Import
import qualified Svod as S

-- | Process request to star\/unstar release and return current status as a
-- JSON value.
--
-- POST request should have @"artist-slug"@ and @"release-slug"@ parameters
-- identifying release as well as 'defaultCsrfParamName' parameter
-- containing CSRF-protection token.

postStarReleaseR :: Handler TypedContent
postStarReleaseR = do
  checkCsrfParamNamed defaultCsrfParamName
  starrer <- fromJust <$> maybeAuthId
  (aslug, rslug) <- runInputPost $ (,)
    <$> ireq textField "artist-slug"
    <*> ireq textField "release-slug"
  releaseViaSlug (mkSlug aslug) (mkSlug rslug) $ \_ release' -> do
    let release = entityKey release'
    active <- runDB (S.toggleStar release starrer)
    n'     <- runDB (S.starCount release)
    let n = fromIntegral n' :: Int
    return . toTypedContent . object $
      ["active" .= active, "count" .= n]
