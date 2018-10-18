module Auth0.Management.Tenants where

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
-- GET /api/v2/tenants/settings

data ChangePassword
  = ChangePassword
  { enabled :: Maybe Bool
  , html    :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON ChangePassword where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance FromJSON ChangePassword where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data GuardianMfaPage
  = GuardianMfaPage
  { enabled :: Maybe Bool
  , html    :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON GuardianMfaPage where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance FromJSON GuardianMfaPage  where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data ErrorPage
  = ErrorPage
  { html        :: Maybe Text
  , showLogLink :: Maybe Bool
  , url         :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON ErrorPage where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance FromJSON ErrorPage where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data Flags
  = Flags
  { changePwdFlowV1         :: Maybe Bool
  , enableApisSection       :: Maybe Bool
  , disableImpersonation    :: Maybe Bool
  , enableClientConnections :: Maybe Bool
  , enablePipeline2         :: Maybe Bool
  } deriving (Generic, Show)

instance ToJSON Flags where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance FromJSON Flags where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data TenantSettingResponse
  = TenantSettingResponse
  { changePassword    :: Maybe ChangePassword
  , guardianMfaPage   :: Maybe GuardianMfaPage
  , defaultAudience   :: Maybe Text
  , defaultDirectory  :: Maybe Text
  , errorPage         :: Maybe ErrorPage
  , flags             :: Maybe Flags
  , friendlyName      :: Maybe Text
  , pictureUrl        :: Maybe Text
  , supportEmail      :: Maybe Text
  , supportUrl        :: Maybe Text
  , allowedLogoutUrls :: Maybe [Text]
  , sessionLifetime   :: Maybe Double
  } deriving (Generic, Show)

instance ToJSON TenantSettingResponse where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance FromJSON TenantSettingResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type TenantSettingsGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Text
  :> Get '[JSON] TenantSettingResponse

tenantSettingsGet ::
     AccessToken
  -> Maybe Text
  -> Maybe Text
  -> ClientM TenantSettingResponse

--------------------------------------------------------------------------------
-- PATCH /api/v2/tenants/settings

type TenantSettingsUpdate = TenantSettingResponse

type TenantSettingsUpdateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] TenantSettingsUpdate
  :> Patch '[JSON] TenantSettingResponse

tenantSettingsUpdate ::
     AccessToken
  -> TenantSettingsUpdate
  -> ClientM TenantSettingResponse

--------------------------------------------------------------------------------

type TenantSettingsApi
  =  "api"
  :> "v2"
  :> "tenants"
  :> "settings"
  :> (    TenantSettingsGetApi
     :<|> TenantSettingsUpdateApi
     )

tenantSettingsApi :: Proxy TenantSettingsApi
tenantSettingsApi = Proxy

tenantSettingsGet
  :<|> tenantSettingsUpdate
  = client tenantSettingsApi
