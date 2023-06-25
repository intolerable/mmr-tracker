module MMRTracker.Server.Stylesheet where

import Data.ByteString.Lazy.Char8 (ByteString)
import Network.HTTP.Media ((//))
import Servant.API.ContentTypes
import Data.Proxy

newtype CSS = CSS ByteString
  deriving (Show, Eq, Ord, Semigroup, Monoid)

instance Accept CSS where
  contentType Proxy = "text" // "css"

instance MimeRender CSS CSS where
  mimeRender Proxy (CSS s) = s

stylesheetHandler :: Monad m => m CSS
stylesheetHandler = pure mempty
