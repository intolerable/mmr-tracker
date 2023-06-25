module MMRTracker.Server.Routes where

import Servant.API
import Servant.HTML.Lucid
import GHC.Generics

import MMRTracker.Server.View
import MMRTracker.Server.Stylesheet

type API = NamedRoutes APIEndpoints

type GetView a = Get '[HTML] (ViewOf a)

data APIEndpoints mode = APIEndpoints
  { index :: mode :- Get '[HTML] (ViewOf IndexView)
  , style :: mode :- "style.css" :> Get '[CSS] CSS
  , adminEndpoints :: mode :- "admin" :> NamedRoutes AdminEndpoints
  } deriving Generic

data AdminEndpoints mode = AdminEndpoints
  { adminIndex :: mode :- Get '[HTML] (ViewOf AdminIndexView)
  } deriving Generic
