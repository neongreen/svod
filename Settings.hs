-- |
-- Module      :  Settings
-- Copyright   :  © 2015 Mark Karpov
-- License     :  GPL-3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Settings are centralized, as much as possible, in this file. This
-- includes database connection settings, static file locations, etc.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Settings where

import ClassyPrelude.Yesod
import Control.Exception (throw)
import Data.Aeson (Result (..), fromJSON, withObject, (.!=), (.:?))
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax (Exp, Name, Q)
import Network.Wai.Handler.Warp (HostPreference)
import Path
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)
import Yesod.Default.Util
  ( WidgetFileSettings
  , widgetFileNoReload
  , widgetFileReload )

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.

data AppSettings = AppSettings
  { appStaticDir :: Path Rel Dir
    -- ^ Directory from which to serve static files
  , appStagingDir :: Path Rel Dir
    -- ^ Where to keep staging releases
  , appReleaseDir :: Path Rel Dir
    -- ^ Where to keep release tarballs
  , appDatabaseConf :: PostgresConf
    -- ^ Configuration settings for accessing the database
  , appRoot :: Text
  -- ^ Base for all generated URLs
  , appHost :: HostPreference
  -- ^ Host/interface the server should bind to
  , appPort :: Int
  -- ^ Port to listen on
  , appIpFromHeader :: Bool
  -- ^ Get the IP address from the header when logging
  , appDetailedRequestLogging :: Bool
  -- ^ Use detailed request logging system
  , appShouldLogAll :: Bool
  -- ^ Should all log messages be displayed?
  , appReloadTemplates :: Bool
  -- ^ Use the reload version of templates
  , appMutableStatic :: Bool
  -- ^ Assume that files in the static dir may change after compilation
  , appSkipCombining :: Bool
  -- ^ Perform no stylesheet/script combining
  , appCopyright :: Text
  -- ^ Copyright text to appear in the footer of the page
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    let ξ = maybe mzero return . parseRelDir
        defaultDev =
#if DEVELOPMENT
          True
#else
          False
#endif
    appStaticDir              <- o .: "static-dir"  >>= ξ
    appStagingDir             <- o .: "staging-dir" >>= ξ
    appReleaseDir             <- o .: "release-dir" >>= ξ
    appDatabaseConf           <- o .: "database"
    appRoot                   <- o .: "approot"
    appHost                   <- fromString <$> o .: "host"
    appPort                   <- o .: "port"
    appIpFromHeader           <- o .: "ip-from-header"

    appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
    appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
    appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
    appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
    appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

    appCopyright              <- o .: "copyright"

    return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.

combineSettings :: CombineSettings
combineSettings = def

----------------------------------------------------------------------------
-- Settings which rarely need changing by a user

widgetFile :: String -> Q Exp
widgetFile =
  (if appReloadTemplates compileTimeAppSettings
   then widgetFileReload
   else widgetFileNoReload)
  widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@.

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.

configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from
-- @config/settings.yml@.

compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
  case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
    Error e -> error e
    Success settings -> settings

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
  (appSkipCombining compileTimeAppSettings)
  combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
  (appSkipCombining compileTimeAppSettings)
  combineSettings
