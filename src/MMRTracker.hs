module MMRTracker where

import MMRTracker.Server
import MMRTracker.Server.Monad
import MMRTracker.Server.Options
import MMRTracker.Database.Migration

import Data.Proxy
import Network.Wai.Handler.Warp
import Servant.Server
import Data.Pool
import Hasql.Connection
import Control.Exception (Exception, throw)

main :: IO ()
main = do
  ServerOptions{..} <- getServerOptions
  connectionPool <- mkConnectionPool postgresOptions

  withResource connectionPool runAllMigrations >>= \case
    Right (Right ()) -> pure ()
    Right (Left err) -> error $ show err
    Left err -> error $ show err

  let Port p = port
  putStrLn $ "running server on port " <> show p
  run (fromIntegral p) (application connectionPool)

application :: Pool Connection -> Application
application connectionPool = do
  let nt :: forall x . App x -> Handler x
      nt = runApp (AppState (Left connectionPool))
  serveWithContext (Proxy @API) context $
    hoistServer (Proxy @API) nt server

context :: Context '[]
context = EmptyContext

mkConnectionPool :: PostgresOptions -> IO (Pool Connection)
mkConnectionPool pgopts = do
  let acquireOrThrow =
        acquire (constructConnectionString pgopts) >>= \case
          Left connectionError ->
            throw $ ConnectionErrorException connectionError
          Right conn -> pure conn
  newPool $ defaultPoolConfig acquireOrThrow release 30 20

newtype ConnectionErrorException =
  ConnectionErrorException ConnectionError
  deriving (Show, Eq, Ord, Read)

instance Exception ConnectionErrorException
