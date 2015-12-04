-- |
-- Module      :  Handler.Release
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Get public information about particular release (HTML or JSON).

{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Release
  ( getReleaseR )
where

import Import

-- | Get public information about particular release in HTML or as JSON.

getReleaseR
  :: Text              -- ^ User slug
  -> Text              -- ^ Release slug
  -> Handler TypedContent
getReleaseR = undefined
