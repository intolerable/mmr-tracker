module MMRTracker.Server.View where

import Control.Monad.Trans.Reader
import GHC.Generics
import Lucid.Base
import Lucid

import MMRTracker.Database.Model.User

data ViewState = ViewState

type View = HtmlT (Reader ViewState) ()

renderView :: Monad m => ViewState -> View -> HtmlT m ()
renderView vs v =
  runReader (commuteHtmlT2 v) vs

class ToView a where
  toView :: a -> View

data ViewOf a = ViewOf ViewState (a -> View) a
  deriving (Generic)

instance ToHtml (ViewOf a) where
  toHtmlRaw = toHtml
  toHtml (ViewOf vs f a) = renderView vs (f a)

viewOf :: ToView a => ViewState -> a -> ViewOf a
viewOf vs a = ViewOf vs toView a

data IndexView = IndexView [User]
  deriving (Show, Eq, Ord)

instance ToView IndexView where
  toView (IndexView us) = do
    h1_ "IndexView"
    p_ $ toHtml $ show us

data UsersFragment = UsersFragment [User]
  deriving (Show, Eq, Ord)

instance ToView UsersFragment where
  toView (UsersFragment _us) = mempty

data LoginView = LoginView [User]
  deriving (Show, Eq, Ord)

instance ToView LoginView where
  toView (LoginView _us) = mempty

data AdminIndexView = AdminIndexView
  deriving (Show, Eq, Ord)

instance ToView AdminIndexView where
  toView AdminIndexView = mempty

