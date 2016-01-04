-- |
-- Module      :  Helper.Path
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Helpers for dealing with paths.

{-# LANGUAGE NoImplicitPrelude #-}

module Helper.Path
  ( getInfoDir
  , getStagingDir
  , getReleaseDir )
where

import Import
import Path
import System.Directory (getCurrentDirectory)

-- | Get absolute path to directory containing info articles.

getInfoDir :: Handler (Path Abs Dir)
getInfoDir = withRelDirSetting (appInfoDir . appSettings)

-- | Get absolute path to staging area.

getStagingDir :: Handler (Path Abs Dir)
getStagingDir = withRelDirSetting (appStagingDir . appSettings)

-- | Get absolute path to directory containing release tarballs.

getReleaseDir :: Handler (Path Abs Dir)
getReleaseDir = withRelDirSetting (appReleaseDir . appSettings)

-- | You tell me how to get relative directory from application data type
-- and I will give you absolute path built from that.

withRelDirSetting :: (App -> Path Rel Dir) -> Handler (Path Abs Dir)
withRelDirSetting getDir = do
  root  <- liftIO getCurrentDirectory >>= parseAbsDir
  sroot <- getDir <$> getYesod
  return (root </> sroot)
