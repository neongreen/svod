-- |
-- Module      :  Widget.Release
-- Copyright   :  © 2015–2016 Mark Karpov
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
import Data.Time
import Helper.Rendering (toInt)
import Import
import Widget.StarRelease (starReleaseW)

-- | Display most important information about given release.

releaseW
  :: Entity Release
  -> Widget
releaseW release = do
  let Release {..} = entityVal release
      toDiffTime = picosecondsToDiffTime . fromIntegral . fromEnum
      placeholder = StaticR $ StaticRoute ["img", "release", "ph_60.jpg"] []
  User {..} <- fromJust <$> φ (get releaseArtist)
  new <- case releaseFinalized of
    Nothing -> return False
    Just releaseDate -> do
      now <- liftIO getCurrentTime
      return $ toDiffTime (diffUTCTime now releaseDate) < noveltyPeriod
  $(widgetFile "release-widget")

-- | For how long a release is considered new?

noveltyPeriod :: DiffTime
noveltyPeriod = secondsToDiffTime (30 * 24 * 60 * 60) -- 30 days
