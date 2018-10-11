module Auth0.Management.ResourceServers where

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

data ResourceServerResponse
  = ResourceServerResponse
  { identifier          :: Maybe Text
  , scopes              :: Maybe [Text]
  , signingAlg          :: Maybe Text
  , signingSecret       :: Maybe Text
  , allowOfflineAccess  :: Maybe Bool
  , skipConsentForVerifiableFirstPartyClients :: Maybe Bool
  , tokenLifetime       :: Maybe Int
  , tokenLifetimeForWeb :: Maybe Int
  , id                  :: Maybe Text
  , name                :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON ResourceServerResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- GET /api/v2/resource-servers

type ResourceServersGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Get '[JSON] [ResourceServerResponse]

resourceServersGet ::
     AccessToken
  -> ClientM [ResourceServerResponse]

--------------------------------------------------------------------------------
-- POST /api/v2/resource-servers

data ResourceServerCreate
  = ResourceServerCreate
  { name                 :: Maybe Text
  , identifier           :: Maybe Text
  , scopes               :: Maybe [Text]
  , signingAlg           :: Maybe Text
  , signingSecret        :: Maybe Text
  , allowOfflineAccess   :: Maybe Bool
  , skipConsentForVerifiableFirstPartyClients :: Maybe Bool
  , verificationKey      :: Maybe Text
  , verificationLocation :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON ResourceServerCreate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type ResourceServerCreateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] ResourceServerCreate
  :> Post '[JSON] [ResourceServerResponse]

resourceServerCreate ::
     AccessToken
  -> ResourceServerCreate
  -> ClientM [ResourceServerResponse]

--------------------------------------------------------------------------------
-- GET /api/v2/resource-servers/{id}

type ResourceServerGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Get '[JSON] ResourceServerResponse

resourceServerGet ::
     AccessToken
  -> Text
  -> ClientM ResourceServerResponse

--------------------------------------------------------------------------------
-- DELETE /api/v2/resource-servers/{id}

type ResourceServerDeleteApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Delete '[JSON] ResourceServerResponse

resourceServerDelete ::
     AccessToken
  -> Text
  -> ClientM ResourceServerResponse

--------------------------------------------------------------------------------
-- PATCH /api/v2/resource-servers/{id}

type ResourceServerUpdate = ResourceServerCreate

type ResourceServerUpdateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> ReqBody '[JSON] ResourceServerUpdate
  :> Patch '[JSON] ResourceServerResponse

resourceServerUpdate ::
     AccessToken
  -> Text
  -> ResourceServerUpdate
  -> ClientM ResourceServerResponse

--------------------------------------------------------------------------------

type ResourceServerApi
  =  "api"
  :> "v2"
  :> "resource-servers"
  :> (    ResourceServersGetApi
     :<|> ResourceServerCreateApi
     :<|> ResourceServerGetApi
     :<|> ResourceServerDeleteApi
     :<|> ResourceServerUpdateApi
     )

resourceServerApi :: Proxy ResourceServerApi
resourceServerApi = Proxy

resourceServersGet
  :<|> resourceServerCreate
  :<|> resourceServerGet
  :<|> resourceServerDelete
  :<|> resourceServerUpdate
  = client resourceServerApi
