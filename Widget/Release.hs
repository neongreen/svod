-- |
-- Module      :  Widget.Release
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Widget displaying info about given release compactly.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widget.Release
  ( releaseW )
where

import Data.Maybe (fromJust)
import Helper.Rendering (toInt)
import Import
import Widget.StarRelease (isStarredBy, starredIcon)
import qualified Svod as S

-- | Display most important information about given release.

releaseW
  :: Entity Release
  -> Widget
releaseW release = do
  let rid = entityKey release
      Release {..} = entityVal release
  User {..} <- fromJust <$> φ (get releaseArtist)
  stars <- φ (S.starCount rid)
  icon  <- starredIcon <$> (ζ maybeAuthId >>= isStarredBy rid)
  $(widgetFile "release-widget")
