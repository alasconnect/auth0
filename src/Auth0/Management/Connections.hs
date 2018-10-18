module Auth0.Management.Connections where

--------------------------------------------------------------------------------
import Data.Aeson hiding (Options)
import Data.Map
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------

-- Response

data ConnectionResponse
  = ConnectionResponse
  { name               :: Maybe Text
  , options            :: Maybe (Map Text Text)
  , id                 :: Maybe Text
  , strategy           :: Maybe Text
  , realms             :: Maybe (Map Text Text)
  , enabledClients     :: Maybe (Map Text Text)
  , isDomainConnection :: Maybe Bool
  , metadata           :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance FromJSON ConnectionResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- GET /api/v2/connections

type ConnectionApi
  =  Header' '[Required] "Authorization" Text
  :> QueryParam' '[Required] "per_page" Int
  :> QueryParam' '[Required] "page" Int
  :> QueryParams "strategy" Text
  :> QueryParam' '[Required] "name" Text
  :> QueryParam' '[Required] "fields" Text
  :> QueryParam' '[Required] "include_fields" Bool
  :> Get '[JSON] [ConnectionResponse]

connection ::
     Text
  -> Int
  -> Int
  -> [Text]
  -> Text
  -> Text
  -> Bool
  -> ClientM [ConnectionResponse]

--------------------------------------------------------------------------------
-- POST /api/v2/connections

-- Request

data Options
  = Options
  { validation             :: Maybe Text
  , passwordPolicy         :: Maybe Text
  , passwordHistory        :: Maybe Text
  , passwordNoPersonalInfo :: Maybe Text
  , passwordDictionary     :: Maybe Text
  , upstreamParams         :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON Options where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data ConnectionCreate
  = ConnectionCreate
  { name           :: Text
  , strategy       :: Text
  , options        :: Maybe Options
  , enabledClients :: Maybe (Map Text Text)
  , realms         :: Maybe (Map Text Text)
  , metadata       :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON ConnectionCreate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type ConnectionCreateApi
  =  Header' '[Required] "Authorization" Text
  :> ReqBody '[JSON] ConnectionCreate
  :> Post '[JSON] ConnectionResponse

connectionCreate ::
     Text
  -> ConnectionCreate
  -> ClientM ConnectionResponse

--------------------------------------------------------------------------------
-- GET /api/v2/connections/{id}

type ConnectionGetApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "connection_id" Text
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Bool
  :> Get '[JSON] ConnectionResponse

connectionGet ::
     Text
  -> Text
  -> Maybe Text
  -> Maybe Bool
  -> ClientM ConnectionResponse

--------------------------------------------------------------------------------
-- DELETE /api/v2/connections/{id}

type ConnectionDeleteApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "connection_id" Text
  :> Delete '[JSON] NoContent

connectionDelete ::
     Text
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- PATCH /api/v2/connections/{id}

data ConnectionUpdate
  = ConnectionUpdate
  { options        :: Maybe Options
  , enabledClients :: Maybe (Map Text Text)
  , realms         :: Maybe (Map Text Text)
  , metadata       :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON ConnectionUpdate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type ConnectionUpdateApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "connection_id" Text
  :> ReqBody '[JSON] ConnectionUpdate
  :> Patch '[JSON] ConnectionResponse

connectionUpdate ::
     Text
  -> Text
  -> ConnectionUpdate
  -> ClientM ConnectionResponse

--------------------------------------------------------------------------------
-- DELETE /api/v2/connections/{id}/users

type ConnectionDeleteUserApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "connection_id" Text
  :> "users"
  :> QueryParam "email" Text
  :> Delete '[JSON] ConnectionResponse

connectionDeleteUser ::
     Text
  -> Text
  -> Maybe Text
  -> ClientM ConnectionResponse

--------------------------------------------------------------------------------

type ConnectionsApi
  =  "api"
  :> "v2"
  :> "connections"
  :> (
          ConnectionApi
     :<|> ConnectionCreateApi
     :<|> ConnectionGetApi
     :<|> ConnectionDeleteApi
     :<|> ConnectionUpdateApi
     :<|> ConnectionDeleteUserApi
     )

connectionsApi :: Proxy ConnectionsApi
connectionsApi = Proxy

connection
  :<|> connectionCreate
  :<|> connectionGet
  :<|> connectionDelete
  :<|> connectionUpdate
  :<|> connectionDeleteUser
  = client connectionsApi
