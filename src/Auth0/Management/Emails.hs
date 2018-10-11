module Auth0.Management.Emails where

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

-- Response

data Credentials
  = Credentials
  { apiUser         :: Maybe Text
  , apiKey          :: Maybe Text
  , accessKeyId     :: Maybe Text
  , secretAccessKey :: Maybe Text
  , region          :: Maybe Text
  , smtpAuth        :: Maybe Text
  , smtpPort        :: Maybe Int
  , smtpUser        :: Maybe Text
  , smtpPass        :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON Credentials where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data EmailProviderResponse
  = EmailProviderResponse
  { name               :: Maybe Text
  , enabled            :: Maybe Bool
  , defaultFromAddress :: Maybe Text
  , credentials        :: Maybe Credentials
  , settings           :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance FromJSON EmailProviderResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- GET /api/v2/emails/provider

type EmailProviderGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Text
  :> Get '[JSON] [EmailProviderResponse]

emailProviderGet ::
     AccessToken
  -> Maybe Text
  -> Maybe Text
  -> ClientM [EmailProviderResponse]

--------------------------------------------------------------------------------
-- DELETE /api/v2/emails/provider

type EmailProviderDeleteApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Delete '[JSON] NoContent

emailProviderDelete ::
     AccessToken
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- PATCH /api/v2/emails/provider

data CredentialsUpdate
  = CredentialsUpdate
  { apiKey :: Text
  } deriving (Generic, Show)

instance ToJSON CredentialsUpdate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data EmailProviderUpdate
  = EmailProviderUpdate
  { name               :: Maybe Text
  , enabled            :: Maybe Bool
  , defaultFromAddress :: Maybe Text
  , credentials        :: Maybe CredentialsUpdate
  , settings           :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON EmailProviderUpdate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type EmailProviderUpdateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] EmailProviderUpdate
  :> Patch '[JSON] EmailProviderResponse

emailProviderUpdate ::
     AccessToken
  -> EmailProviderUpdate
  -> ClientM EmailProviderResponse

--------------------------------------------------------------------------------
-- POST /api/v2/emails/provider

type EmailProviderCreate = EmailProviderUpdate

type EmailProviderCreateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] EmailProviderCreate
  :> Post '[JSON] EmailProviderResponse

emailProviderCreate ::
     AccessToken
  -> EmailProviderCreate
  -> ClientM EmailProviderResponse

--------------------------------------------------------------------------------

type EmailsApi
  =  "api"
  :> "v2"
  :> "emails"
  :> (
          EmailProviderGetApi
     :<|> EmailProviderDeleteApi
     :<|> EmailProviderUpdateApi
     :<|> EmailProviderCreateApi
     )

emailsApi :: Proxy EmailsApi
emailsApi = Proxy

emailProviderGet
  :<|> emailProviderDelete
  :<|> emailProviderUpdate
  :<|> emailProviderCreate
  = client emailsApi
