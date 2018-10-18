module Auth0.Management.Stats where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/stats/active-users

type StatsActiveUsersGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Get '[JSON] Int

statsActiveUsersGet ::
     AccessToken
  -> ClientM Int

--------------------------------------------------------------------------------
-- GET /api/v2/stats/daily

data StatsDailyResponse
  = StatsDailyResponse
  { date   :: Maybe Text
  , logins :: Maybe Int
  } deriving (Generic, Show)

instance FromJSON StatsDailyResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type StatsDailyGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> QueryParam "from" Text
  :> QueryParam "to" Text
  :> Get '[JSON] [StatsDailyResponse]

statsDailyGet ::
     AccessToken
  -> Maybe Text
  -> Maybe Text
  -> ClientM [StatsDailyResponse]

--------------------------------------------------------------------------------

type StatsApi
  =  "api"
  :> "v2"
  :> "stats"
  :> (    "active-users" :> StatsActiveUsersGetApi
     :<|> "daily" :> StatsDailyGetApi
     )

statsApi :: Proxy StatsApi
statsApi = Proxy

statsActiveUsersGet
  :<|> statsDailyGet
  = client statsApi
