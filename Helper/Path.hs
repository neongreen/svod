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
  , getFConfig )
where

import Import
import Path
import Path.IO
import Svod.LTS (FConfig, mkFConfig)

-- | Get absolute path to directory containing info articles.

getInfoDir :: Handler (Path Abs Dir)
getInfoDir = withRelDirSetting (appInfoDir . appSettings)

-- | Get 'FConfig' configuration.

getFConfig :: Handler FConfig
getFConfig = mkFConfig <$> withRelDirSetting (appContentDir . appSettings)

-- | You tell me how to get relative directory from application data type
-- and I will give you absolute path built from that.

withRelDirSetting :: (App -> Path Rel Dir) -> Handler (Path Abs Dir)
withRelDirSetting getDir = do
  root  <- getCurrentDir
  sroot <- getDir <$> getYesod
  return (root </> sroot)
