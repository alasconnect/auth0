module Auth0.Management.DeviceCredentials where

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
-- GET /api/v2/device-credentials

data DeviceCredentialResponse
  = DeviceCredentialResponse
  { id         :: Text
  , deviceName :: Text
  , deviceId   :: Text
  , ctype      :: Text
  , userId     :: Text
  } deriving (Generic, Show)

instance FromJSON DeviceCredentialResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = f }
    where
      f "type" = "ctype"
      f v      = camelTo2 '_' v

type DeviceCredentialGetApi
  =  Header' '[Required] "Authorization" Text
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Bool
  :> QueryParam "user_id" Text
  :> QueryParam "client_id" ClientId
  :> QueryParam "type" Text
  :> Get '[JSON] [DeviceCredentialResponse]

deviceCredentialGet ::
     Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> Maybe ClientId
  -> Maybe Text
  -> ClientM [DeviceCredentialResponse]

--------------------------------------------------------------------------------
-- POST /api/v2/device-credentials

data DeviceCredentialCreate
  = DeviceCredentialCreate
  { deviceName :: Text
  , ctype      :: Text
  , value      :: Text
  , deviceId   :: Text
  , clientId   :: ClientId
  } deriving (Generic, Show)

instance ToJSON DeviceCredentialCreate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = f }
    where
      f "ctype" = "type"
      f v       = camelTo2 '_' v

data DeviceCredentialId
  = DeviceCredentialId
  { id :: Text
  } deriving (Generic, Show)

instance FromJSON DeviceCredentialId

type DeviceCredentialCreateApi
  =  Header' '[Required] "Authorization" Text
  :> ReqBody '[JSON] DeviceCredentialCreate
  :> Post '[JSON] DeviceCredentialId

deviceCredentialCreate ::
     Text
  -> DeviceCredentialCreate
  -> ClientM DeviceCredentialId

--------------------------------------------------------------------------------
-- DELETE /api/v2/device-credentials/{id}

type DeviceCredentialDeleteApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "device_id" Text
  :> Delete '[JSON] DeviceCredentialId

deviceCredentialDelete ::
     Text
  -> Text
  -> ClientM DeviceCredentialId

--------------------------------------------------------------------------------

type DeviceCredentialsApi
  =  "api"
  :> "v2"
  :> "device-credentials"
  :> (DeviceCredentialGetApi :<|> DeviceCredentialCreateApi :<|> DeviceCredentialDeleteApi)

deviceCredentialsApi :: Proxy DeviceCredentialsApi
deviceCredentialsApi = Proxy

deviceCredentialGet
  :<|> deviceCredentialCreate
  :<|> deviceCredentialDelete
  = client deviceCredentialsApi
