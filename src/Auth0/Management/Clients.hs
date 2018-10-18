module Auth0.Management.Clients where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Map
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------
import Auth0.Types
--------------------------------------------------------------------------------

-- Response types

data JwtConfiguration
  = JwtConfiguration
  { lifetimeInSeconds :: Maybe Int
  , secretEncoded     :: Maybe Bool
  , scopes            :: Maybe (Map Text Text)
  , alg               :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON JwtConfiguration where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance ToJSON JwtConfiguration where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data EncryptionKey
  = EncryptionKey
  { pub     :: Maybe Text
  , cert    :: Maybe Text
  , subject :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON EncryptionKey where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance ToJSON EncryptionKey where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data Android
  = Android
  { appPackageName         :: Text
  , sha256CertFingerprints :: [Text]
  } deriving (Generic, Show)

instance FromJSON Android where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance ToJSON Android where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data IOS
  = IOS
  { teamId              :: Text
  , appBundleIdentifier :: Text
  } deriving (Generic, Show)

instance FromJSON IOS where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance ToJSON IOS where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data Mobile
  = Mobile
  { android :: Maybe Android
  , ios     :: Maybe IOS
  } deriving (Generic, Show)

instance FromJSON Mobile
instance ToJSON Mobile

data ClientResponse
  = ClientResponse
  { name                    :: Maybe Text
  , description             :: Maybe Text
  , clientId                :: Maybe ClientId
  , clientSecret            :: Maybe ClientSecret
  , appType                 :: Maybe Text
  , logoUri                 :: Maybe Text
  , isFirstParty            :: Bool
  , oidcConformant          :: Bool
  , callbacks               :: Maybe [Text]
  , allowedOrigins          :: Maybe [Text]
  , webOrigins              :: Maybe [Text]
  , clientAliases           :: Maybe [Text]
  , allowedClients          :: Maybe [Text]
  , allowedLoginUrls        :: Maybe [Text]
  , jwtConfiguration        :: Maybe JwtConfiguration
  , signingKeys             :: Maybe [Text]
  , encryptionKey           :: Maybe EncryptionKey
  , sso                     :: Maybe Bool
  , ssoDisabled             :: Maybe Bool
  , crossOriginAuth         :: Maybe Bool
  , crossOriginLoc          :: Maybe Text
  , customLoginPageOn       :: Maybe Bool
  , customLoginPage         :: Maybe Text
  , customLoginPagePreview  :: Maybe Text
  , formTemplate            :: Maybe Text
  , addons                  :: Maybe (Map Text Text)
  , tokenEndpointAuthMethod :: Maybe Text
  , clientMetadata          :: Maybe (Map Text Text)
  , mobile                  :: Maybe Mobile
  } deriving (Generic, Show)

instance FromJSON ClientResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- GET /api/v2/clients

type ClientsGetApi
  =  Header' '[Required] "Authorization" Text
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Bool
  :> QueryParam "page" Int
  :> QueryParam "per_page" Int
  :> QueryParam "include_totals" Bool
  :> Get '[JSON] [ClientResponse]

clientsGet ::
     Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe Bool
  -> ClientM [ClientResponse]

--------------------------------------------------------------------------------
-- POST /api/v2/clients

data ClientCreate
  = ClientCreate
  { name                    :: Maybe Text
  , description             :: Maybe Text
  , clientId                :: Maybe ClientId
  , clientSecret            :: Maybe ClientSecret
  , logoUri                 :: Maybe Text
  , callbacks               :: Maybe [Text]
  , allowedOrigins          :: Maybe [Text]
  , webOrigins              :: Maybe [Text]
  , clientAliases           :: Maybe [Text]
  , allowedClients          :: Maybe [Text]
  , allowedLogoutUrls       :: Maybe [Text]
  , grantTypes              :: Maybe [Text]
  , tokenEndpointAuthMethod :: Maybe Text
  , oidcConformant          :: Bool
  , jwtConfiguration        :: Maybe JwtConfiguration
  , encryptionKey           :: Maybe EncryptionKey
  , appType                 :: Maybe Text
  , sso                     :: Maybe Bool
  , crossOriginAuth         :: Maybe Bool
  , crossOriginLoc          :: Maybe Text
  , ssoDisabled             :: Maybe Bool
  , customLoginPageOn       :: Maybe Bool
  , customLoginPage         :: Maybe Text
  , customLoginPagePreview  :: Maybe Text
  , formTemplate            :: Maybe Text
  , isHerokuApp             :: Maybe Bool
  , addons                  :: Maybe (Map Text Text)
  , clientMetadata          :: Maybe (Map Text Text)
  , mobile                  :: Maybe Mobile
  } deriving (Generic, Show)

instance ToJSON ClientCreate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type ClientsPostApi
  =  Header' '[Required] "Authorization" Text
  :> ReqBody '[JSON] ClientCreate
  :> Post '[JSON] ClientResponse

clientsPost ::
     Text
  -> ClientCreate
  -> ClientM ClientResponse

--------------------------------------------------------------------------------
-- GET /api/v2/clients/{id}

type ClientGetApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "client_id" Text
  :> QueryParam' '[Required] "id" Text
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Bool
  :> Get '[JSON] ClientResponse

clientGet ::
     Text
  -> Text
  -> Text
  -> Maybe Text
  -> Maybe Bool
  -> ClientM ClientResponse

--------------------------------------------------------------------------------
-- DELETE /api/v2/clients/{id}

type ClientDeleteApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "client_id" Text
  :> Delete '[JSON] NoContent

clientDelete ::
     Text
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- PATCH /api/v2/clients/{id}

type ClientUpdate = ClientCreate

type ClientUpdateApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "client_id" Text
  :> Patch '[JSON] NoContent

clientUpdate ::
     Text
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- POST /api/v2/clients/{id}/rotate-secret

type ClientRotateSecretApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "client_id" Text
  :> "rotate-secret"
  :> Post '[JSON] ClientResponse

clientRotateSecret ::
     Text
  -> Text
  -> ClientM ClientResponse

--------------------------------------------------------------------------------

type ClientsApi
  =  "api"
  :> "v2"
  :> "clients"
  :> (    ClientsGetApi
     :<|> ClientsPostApi
     :<|> ClientGetApi
     :<|> ClientDeleteApi
     :<|> ClientUpdateApi
     :<|> ClientRotateSecretApi
     )

clientsApi :: Proxy ClientsApi
clientsApi = Proxy

clientsGet
  :<|> clientsPost
  :<|> clientGet
  :<|> clientDelete
  :<|> clientUpdate
  :<|> clientRotateSecret
  = client clientsApi
