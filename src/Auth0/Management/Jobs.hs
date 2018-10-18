module Auth0.Management.Jobs where

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

--------------------------------------------------------------------------------
-- GET /api/v2/jobs/{id}

-- Response

data JobResponse
  = JobResponse
  { status          :: Text
  , jtype           :: Text
  , createdAt       :: Maybe Text
  , id              :: Text
  , connectionId    :: Maybe Text
  , location        :: Maybe Text
  , percentageDone  :: Maybe Int
  , timeLeftSeconds :: Maybe Int
  , fields          :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance FromJSON JobResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = f }
    where
      f "type" = "jtype"
      f v      = camelTo2 '_' v

instance ToJSON JobResponse where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = f }
    where
      f "jtype" = "type"
      f v       = camelTo2 '_' v

type JobsGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Get '[JSON] JobResponse

jobsGet ::
     AccessToken
  -> Text
  -> ClientM JobResponse

--------------------------------------------------------------------------------
-- GET /api/v2/jobs/{id}/errors

-- TODO: This looks like it should return something
type JobsErrorsGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> "errors"
  :> Get '[JSON] NoContent

jobsErrorsGet ::
     AccessToken
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- GET /api/v2/jobs/{id}/results

data JobResultsResponse
  = JobResultsResponse
  { email    :: Maybe Text
  , username :: Maybe Text
  , matched  :: Maybe Bool
  , exist    :: Maybe Bool
  } deriving (Generic, Show)

instance FromJSON JobResultsResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type JobsResultsGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> "results"
  :> Get '[JSON] JobResultsResponse

jobsResultsGet ::
     AccessToken
  -> Text
  -> ClientM JobResultsResponse

--------------------------------------------------------------------------------
-- POST /api/v2/jobs/users-exports

data JobExportCreate
  = JobExportCreate
  { connectionId :: Maybe Text
  , format       :: Maybe Text
  , limit        :: Maybe Int
  , fields       :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON JobExportCreate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type JobsUsersExportsCreateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] JobExportCreate
  :> Post '[JSON] JobResponse

jobsUsersExportsCreate ::
     AccessToken
  -> JobExportCreate
  -> ClientM JobResponse

--------------------------------------------------------------------------------
-- POST /api/v2/jobs/users-imports

-- TODO: Requires users file uploaded
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- POST /api/v2/jobs/verification-email

data JobVerificationEmail
  = JobVerificationEmail
  { userId   :: Text
  , clientId :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON JobVerificationEmail where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data JobVerificationEmailResponse
  = JobVerificationEmailResponse
  { status    :: Text
  , jtype     :: Text
  , createdAt :: Maybe Text
  , id        :: Text
  } deriving (Generic, Show)

instance FromJSON JobVerificationEmailResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = f }
    where
      f "type" = "jtype"
      f v      = camelTo2 '_' v

type JobsVerificationEmailCreateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] JobVerificationEmail
  :> Post '[JSON] JobVerificationEmailResponse

jobsVerificationEmailCreate ::
     AccessToken
  -> JobVerificationEmail
  -> ClientM JobVerificationEmailResponse

--------------------------------------------------------------------------------

type JobsApi
  =  "api"
  :> "v2"
  :> "jobs"
  :> (    JobsGetApi
     :<|> JobsErrorsGetApi
     :<|> JobsResultsGetApi
     :<|> JobsUsersExportsCreateApi
     :<|> JobsVerificationEmailCreateApi
     )

jobsApi :: Proxy JobsApi
jobsApi = Proxy

jobsGet
  :<|> jobsErrorsGet
  :<|> jobsResultsGet
  :<|> jobsUsersExportsCreate
  :<|> jobsVerificationEmailCreate
  = client jobsApi
