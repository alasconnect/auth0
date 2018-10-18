module Auth0.Management.Guardian where

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
-- GET /api/v2/guardian/factors

data Guardian
  = Guardian
  { enabled      :: Bool
  , trialExpired :: Maybe Bool
  , name         :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON Guardian where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GuardianFactorsGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Get '[JSON] [Guardian]

guardianFactorsGet ::
     AccessToken
  -> ClientM [Guardian]

--------------------------------------------------------------------------------
-- PUT /api/v2/guardian/factors/{name}

data GuardianFactorUpdate
  = GuardianFactorUpdate
  { enabled :: Bool
  } deriving (Generic, Show)

instance FromJSON GuardianFactorUpdate where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance ToJSON GuardianFactorUpdate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GuardianFactorsUpdateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "name" Text
  :> ReqBody '[JSON] GuardianFactorUpdate
  :> Put '[JSON] GuardianFactorUpdate

guardianFactorsUpdate ::
     AccessToken
  -> Text
  -> GuardianFactorUpdate
  -> ClientM GuardianFactorUpdate

--------------------------------------------------------------------------------
-- GET /api/v2/guardian/factors/sms/templates

data Template
  = Template
  { enrollmentMessage   :: Text
  , verificationMessage :: Text
  } deriving (Generic, Show)

instance FromJSON Template where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance ToJSON Template where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GuardianFactorsSmsTemplatesGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Get '[JSON] [Template]

guardianFactorsSmsTemplatesGet ::
     AccessToken
  -> ClientM [Template]

--------------------------------------------------------------------------------
-- PUT /api/v2/guardian/factors/sms/templates

type GuardianFactorsSmsTemplatesUpdateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Put '[JSON] Template

guardianFactorsSmsTemplatesUpdate ::
     AccessToken
  -> ClientM Template

--------------------------------------------------------------------------------
-- GET /api/v2/guardian/factors/push-notification/providers/sns

data PushNotification
  = PushNotification
  { awsAccessKeyId                :: Maybe Text
  , awsSecretAccessKey            :: Maybe Text
  , awsRegion                     :: Maybe Text
  , snsApnsPlatformApplicationArn :: Maybe Text
  , snsGcmPlatformApplicationArn  :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON PushNotification where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GuardianPushNotificationGetApi
  =  "push-notification"
  :> "providers"
  :> "sns"
  :> Header' '[Required] "Authorization" AccessToken
  :> Get '[JSON] PushNotification

guardianPushNotificationGet ::
     AccessToken
  -> ClientM PushNotification

--------------------------------------------------------------------------------
-- GET /api/v2/guardian/enrollments/{id}

data Enrollment
  = Enrollment
  { id          :: Maybe Text
  , status      :: Maybe Text
  , name        :: Maybe Text
  , identifier  :: Maybe Text
  , phoneNumber :: Maybe Text
  , enrolledAt  :: Maybe Text
  , lastAuth    :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON Enrollment where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GuardianEnrollmentsGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Get '[JSON] [Enrollment]

guardianEnrollmentsGet ::
     AccessToken
  -> ClientM [Enrollment]

--------------------------------------------------------------------------------
-- DELETE /api/v2/guardian/enrollments/{id}

type GuardianEnrollmentsDeleteApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Delete '[JSON] NoContent

guardianEnrollmentsDelete ::
     AccessToken
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- TODO: Twilio
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- POST /api/v2/guardian/enrollments/ticket

data GuardianEnrollmentTicket
  = GuardianEnrollmentTicket
  { userId   :: Text
  , email    :: Maybe Text
  , sendMail :: Maybe Bool
  } deriving (Generic, Show)

instance ToJSON GuardianEnrollmentTicket where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data GuardianEnrollmentTicketResponse
  = GuardianEnrollmentTicketResponse
  { ticketId  :: Maybe Text
  , ticketUrl :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON GuardianEnrollmentTicketResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GuardianEnrollmentTicketCreateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] GuardianEnrollmentTicket
  :> Post '[JSON] GuardianEnrollmentTicketResponse

guardianEnrollmentTicketCreate ::
     AccessToken
  -> GuardianEnrollmentTicket
  -> ClientM GuardianEnrollmentTicketResponse

--------------------------------------------------------------------------------

type GuardianFactorsApi
  = "factors" :>
    (
         (GuardianFactorsGetApi :<|> GuardianFactorsUpdateApi :<|> GuardianPushNotificationGetApi)
    :<|> ("sms" :> "templates" :> (GuardianFactorsSmsTemplatesGetApi :<|> GuardianFactorsSmsTemplatesUpdateApi))
    )

type GuardianEnrollmentsApi
  = "enrollments" :>
    (
         (GuardianEnrollmentsGetApi :<|> GuardianEnrollmentsDeleteApi)
    :<|> ("ticket" :> GuardianEnrollmentTicketCreateApi)
    )

type GuardianApi
  =  "api"
  :> "v2"
  :> "guardian"
  :> (GuardianFactorsApi :<|> GuardianEnrollmentsApi)

guardianFactorsApi :: Proxy GuardianFactorsApi
guardianFactorsApi = Proxy

guardianEnrollmentsApi :: Proxy GuardianEnrollmentsApi
guardianEnrollmentsApi = Proxy

guardianApi :: Proxy GuardianApi
guardianApi = Proxy

((guardianFactorsGet :<|> guardianFactorsUpdate :<|> guardianPushNotificationGet)
       :<|> (guardianFactorsSmsTemplatesGet :<|> guardianFactorsSmsTemplatesUpdate))
  :<|> ((guardianEnrollmentsGet :<|> guardianEnrollmentsDelete)
       :<|> guardianEnrollmentTicketCreate)
  = client guardianApi
