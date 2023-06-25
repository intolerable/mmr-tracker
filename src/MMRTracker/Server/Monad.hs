module MMRTracker.Server.Monad
  ( App
  , runApp
  , AppState(..)
  , withConnection
  , runSelectMany
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Servant.Server
import Control.Monad.Trans.Reader
import Hasql.Connection (Connection)
import Data.Pool
import Hasql.Session (run, statement)
import Rel8

data AppState = AppState
  { appStateConnection :: Either (Pool Connection) Connection
  }

newtype App a = App (ReaderT AppState Handler a)
  deriving (Functor, Applicative, Monad, MonadIO)

runApp :: AppState -> App a -> Handler a
runApp st (App a) = runReaderT a st

withConnection :: (Connection -> App a) -> App a
withConnection f = do
  App (asks appStateConnection) >>= \case
    Left pool -> App $
      liftBaseOp (withResource pool) \conn -> do
        let App r = f conn
        appState <- ask
        lift $ runReaderT r appState { appStateConnection = Right conn }
    Right conn -> f conn

runSelectMany :: ToExprs exprs (FromExprs exprs)
              => Query exprs -> App [FromExprs exprs]
runSelectMany q = do
  queryResult <- withConnection \conn ->
    liftIO (run (statement () (select q)) conn)
  case queryResult of
    Left err -> error $ show err
    Right res -> pure res
