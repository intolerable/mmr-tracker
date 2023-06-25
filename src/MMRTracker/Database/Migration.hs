module MMRTracker.Database.Migration where

import Control.Monad
import Control.Monad.Trans.Except
import Hasql.Connection
import Hasql.Migration
import Hasql.Session
import Hasql.Transaction.Sessions
import Paths_mmr_tracker
import System.FilePath

runAllMigrations :: Connection
                 -> IO (Either QueryError (Either MigrationError ()))
runAllMigrations conn = do
  migrations <- loadMigrationsFromResources
  executeMigrations conn (MigrationInitialization : migrations)

executeMigrations :: Connection
                  -> [MigrationCommand]
                  -> IO (Either QueryError (Either MigrationError ()))
executeMigrations conn cs = do
  let act =
        transaction Serializable Write do
          runExceptT $ forM_ cs \c -> ExceptT do
            maybe (Right ()) Left <$> runMigration c
  run act conn

loadMigrationsFromResources :: IO [MigrationCommand]
loadMigrationsFromResources = do
  dataDir <- getDataDir
  let migrationsDir = dataDir </> "resources" </> "migrations"
  loadMigrationsFromDirectory migrationsDir
