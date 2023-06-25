module MMRTracker.Server.Options where

import Fmt
import Data.ByteString.Char8 (ByteString)
import Options.Applicative
import Data.Word
import Data.Maybe
import qualified Hasql.Connection as Hasql
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text.Encoding as Text

getServerOptions :: IO ServerOptions
getServerOptions =
  execParser $
    info (optionsParser <**> helper) mempty

data ServerOptions = ServerOptions
  { port :: Port
  , postgresOptions :: PostgresOptions
  } deriving (Show, Eq, Ord)

optionsParser :: Parser ServerOptions
optionsParser =
  ServerOptions <$> portParser
                <*> postgresOptionsParser

newtype Port = Port Word16
  deriving (Show, Eq, Ord)

instance Buildable Port where
  build (Port n) = build n

portParser :: Parser Port
portParser =
  Port <$> option auto (long "port" <> short 'p')

data PostgresOptions = PostgresOptions
  { postgresDatabaseName :: ByteString
  , postgresPort :: Port
  , postgresHost :: Maybe ByteString
  , postgresUser :: Maybe ByteString
  , postgresPassword :: Maybe ByteString
  } deriving (Show, Eq, Ord)

postgresOptionsParser :: Parser PostgresOptions
postgresOptionsParser =
  PostgresOptions <$> postgresNameParser
                  <*> postgresPortParser
                  <*> postgresHostParser
                  <*> postgresUserParser
                  <*> postgresPasswordParser
  where
    postgresNameParser = strOption (long "pg-dbname")
    postgresPortParser = Port <$> option auto (long "pg-port")
    postgresHostParser = optional $ strOption (long "pg-host")
    postgresUserParser = optional $ strOption (long "pg-user")
    postgresPasswordParser = optional $ strOption (long "pg-pass")

constructConnectionString :: PostgresOptions -> Hasql.Settings
constructConnectionString PostgresOptions{..} =
  ByteString.intercalate " " $ catMaybes $
    [ pure $ "dbname="+|bsF postgresDatabaseName|+""
    , pure $ "port="+|postgresPort|+""
    , fmap (\h -> "host="+|bsF h|+"") postgresHost
    , fmap (\u -> "user="+|bsF u|+"") postgresUser
    , fmap (\p -> "password="+|bsF p|+"") postgresPassword
    ]
  where
    bsF = build . Text.decodeUtf8
