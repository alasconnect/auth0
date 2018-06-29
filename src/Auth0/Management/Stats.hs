{-# LANGUAGE DeriveGeneric #-}

module Auth0.Management.Stats where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Text
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/stats/active-users

runGetStatsActiveUsers
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> m (Auth0Response Int)
runGetStatsActiveUsers (TokenAuth tenant accessToken) =
  let api = API Get "/api/v2/stats/active-users"
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- GET /api/v2/stats/daily

data StatsDaily
  = StatsDaily
  { from :: Text
  , to   :: Text
  } deriving (Show)

instance ToRequest StatsDaily where
  toRequest (StatsDaily a b) =
    [ toField "from" a
    , toField "to" b
    ]

data StatsDailyResponse
  = StatsDailyResponse
  { date   :: Maybe Text
  , logins :: Maybe Int
  } deriving (Generic, Show)

instance FromJSON StatsDailyResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetStatsDaily
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> StatsDaily -> m (Auth0Response [StatsDailyResponse])
runGetStatsDaily (TokenAuth tenant accessToken) o =
  let api = API Get "/api/v2/stats/daily"
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])
