-- |
-- Module      :  Handler.Releases
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Public information about releases (HTML and JSON).

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Releases
  ( getReleasesR )
where

import Import

-- | Get list of releases either in paginated form (HTML), or as JSON.

getReleasesR :: Handler TypedContent
getReleasesR = undefined
