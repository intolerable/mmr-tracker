module MMRTracker.Server
  ( API
  , server
  ) where

import Servant.API
import Servant.Server
import Rel8

import MMRTracker.Database
import MMRTracker.Server.Monad
import MMRTracker.Server.Routes
import MMRTracker.Server.View
import MMRTracker.Server.Stylesheet

server :: ServerT API App
server = APIEndpoints
  { index = indexHandler
  , style = stylesheetHandler
  , adminEndpoints = adminEndpointsHandlers
  }

indexHandler :: App (ViewOf IndexView)
indexHandler = do
  users <- runSelectMany (each usersSchema)
  pure $ viewOf ViewState (IndexView users)

adminEndpointsHandlers :: ServerT (NamedRoutes AdminEndpoints) App
adminEndpointsHandlers = AdminEndpoints
  { adminIndex = pure AdminIndexView
  }
