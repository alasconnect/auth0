{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Jobs where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Map
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
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
    genericParseJSON defaultOptions { fieldLabelModifier = f }
    where
      f "text" = "jtype"
      f v      = camelTo2 '_' v

instance ToJSON JobResponse where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = f }
    where
      f "jtext" = "type"
      f v       = camelTo2 '_' v

runGetJob
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response JobResponse)
runGetJob (TokenAuth tenant accessToken) i =
  let api = API Get ("/api/v2/jobs/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- GET /api/v2/jobs/{id}/errors

runGetJobErrors
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response ())
runGetJobErrors (TokenAuth tenant accessToken) i =
  let api = API Get ("/api/v2/jobs/" <> encodeUtf8 i <> "/errors")
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

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
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetJobResults
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response JobResultsResponse)
runGetJobResults (TokenAuth tenant accessToken) i =
  let api = API Get ("/api/v2/jobs/" <> encodeUtf8 i <> "/results")
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateJobExport
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> JobExportCreate -> m (Auth0Response JobResponse)
runCreateJobExport (TokenAuth tenant accessToken) o =
  let api = API Post "/api/v2/jobs/users-exports"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data JobVerificationEmailResponse
  = JobVerificationEmailResponse
  { status    :: Text
  , jtype     :: Text
  , createdAt :: Maybe Text
  , id        :: Text
  } deriving (Generic, Show)

instance FromJSON JobVerificationEmailResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateJobVerificationEmail
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> JobVerificationEmail -> m (Auth0Response JobVerificationEmailResponse)
runCreateJobVerificationEmail (TokenAuth tenant accessToken) o =
  let api = API Post "/api/v2/jobs/verification-email"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])
