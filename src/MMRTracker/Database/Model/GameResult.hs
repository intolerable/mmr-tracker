module MMRTracker.Database.Model.GameResult where

import Rel8
import GHC.Generics
import Data.UUID
import Data.Int
import Data.Time.Clock
import Data.Functor.Identity

import MMRTracker.Database.Model.User

newtype GameResultID = GameResultID UUID
  deriving (Show, Read, Eq, Ord, DBType, DBEq)

newtype MatchID = MatchID Int64
  deriving (Show, Read, Eq, Ord, DBType, DBEq)

data GameResultF f = GameResultF
  { gameResultID :: f GameResultID
  , createdAt :: f UTCTime
  , mmrAfter :: f (Maybe Int32)
  , partySize :: f (Maybe Int32)
  , matchID :: f (Maybe MatchID)
  , gameResultUserID :: f UserID
  } deriving (Generic)

deriving instance f ~ Identity => Show (GameResultF f)
deriving instance f ~ Identity => Eq (GameResultF f)

instance Rel8able GameResultF

gameResultsSchema :: TableSchema (GameResultF Name)
gameResultsSchema = TableSchema
  { name = "game_results"
  , schema = Nothing
  , columns = GameResultF
    { gameResultID = "id"
    , createdAt = "created_at"
    , mmrAfter = "mmr_after"
    , partySize = "party_size"
    , matchID = "match_id"
    , gameResultUserID = "user_id"
    }
  }
