module MMRTracker.Database.Model.User where

import Data.Functor.Identity
import GHC.Generics
import Data.UUID
import Rel8
import Data.Text (Text)

newtype UserID = UserID UUID
  deriving (Show, Read, Eq, Ord, DBType)

newtype Username = Username Text
  deriving (Show, Read, Eq, Ord, DBType)

type User = UserF Identity

data UserF f = UserF
  { userID :: f UserID
  , username :: f Username
  } deriving (Generic)

deriving instance f ~ Identity => Show (UserF f)
deriving instance f ~ Identity => Eq (UserF f)
deriving instance f ~ Identity => Ord (UserF f)

instance Rel8able UserF

usersSchema :: TableSchema (UserF Name)
usersSchema = TableSchema
  { name = "users"
  , schema = Nothing
  , columns = UserF
    { userID = "id"
    , username = "username"
    }
  }
