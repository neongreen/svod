-- |
-- Module      :  TestImport
-- Copyright   :  © 2015–2016 Mark Karpov
-- License     :  GNU GPL version 3
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Some helpers for testing.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module TestImport
  ( module TestImport
  , module X )
where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X
import Database.Persist      as X hiding (get, delete, deleteBy)
import Database.Persist.Sql
  ( SqlPersistM
  , SqlBackend
  , runSqlPersistMPool
  , rawExecute
  , rawSql
  , unSingle
  , connEscapeName )
import Foundation            as X
import Svod.Model            as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
  app <- getTestYesod
  liftIO (runDBWithApp app query)

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  settings <- loadAppSettings
    ["config/test-settings.yml", "config/settings.yml"]
    []
    ignoreEnv
  foundation <- makeFoundation settings
  wipeDB foundation
  logWare <- liftIO (makeLogWare foundation)
  return (foundation, logWare)

-- | This function will truncate all of the tables in database. 'withApp'
-- calls it before each test, creating a clean environment for each spec to
-- run in.

wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
  tables <- getTables
  sqlBackend <- ask
  let escapedTables = (connEscapeName sqlBackend . DBName) <$> tables
      query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
  rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
  tables <- rawSql [st|
    SELECT table_name
    FROM information_schema.tables
    WHERE table_schema = 'public';
  |] []
  return (unSingle <$> tables)
