{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Auth0.Management.Guardian where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/guardian/factors

-- Response

data Guardian
  = Guardian
  { enabled      :: Bool
  , trialExpired :: Maybe Bool
  , name         :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON Guardian where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetGuardians
  :: (MonadIO m, MonadThrow m)
  => Auth -> m (Auth0Response [Guardian])
runGetGuardians a =
  let api = API Get "/api/v2/guardian/factors"
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

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
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetGuardianEnrollments
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> m (Auth0Response [Enrollment])
runGetGuardianEnrollments a i =
  let api = API Get ("/api/v2/guardian/enrollments/" <> encodeUtf8 i)
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- DELETE /api/v2/guardian/enrollments/{id}

runDeleteGuardianEnrollment
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> m (Auth0Response ())
runDeleteGuardianEnrollment a i =
  let api = API Delete ("/api/v2/guardian/enrollments/" <> encodeUtf8 i)
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- GET /api/v2/guardian/factors/sms/templates

data Template
  = Template
  { enrollmentMessage   :: Text
  , verificationMessage :: Text
  } deriving (Generic, Show)

instance FromJSON Template where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Template where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetGuardianTemplate
  :: (MonadIO m, MonadThrow m)
  => Auth -> m (Auth0Response Template)
runGetGuardianTemplate a =
  let api = API Get "/api/v2/guardian/factors/sms/templates"
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- PUT /api/v2/guardian/factors/sms/templates

runUpdateGuardianTemplate
  :: (MonadIO m, MonadThrow m)
  => Auth -> Template -> m (Auth0Response Template)
runUpdateGuardianTemplate a o =
  let api = API Put "/api/v2/guardian/factors/sms/templates"
  in execRequest a api (Nothing :: Maybe ()) (Just o) Nothing

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
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetGuardianPushNotification
  :: (MonadIO m, MonadThrow m)
  => Auth -> m (Auth0Response PushNotification)
runGetGuardianPushNotification a =
  let api = API Get "/api/v2/guardian/factors/push-notifications/providers/sns"
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data GuardianEnrollmentTicketResponse
  = GuardianEnrollmentTicketResponse
  { ticketId  :: Maybe Text
  , ticketUrl :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON GuardianEnrollmentTicketResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateGuardianEnrollmentTicket
  :: (MonadIO m, MonadThrow m)
  => Auth -> GuardianEnrollmentTicket -> m (Auth0Response GuardianEnrollmentTicketResponse)
runCreateGuardianEnrollmentTicket a o =
  let api = API Post "/api/v2/guardian/encrollments/ticket"
  in execRequest a api (Nothing :: Maybe ()) (Just o) Nothing

--------------------------------------------------------------------------------
-- PUT /api/v2/guardian/factors/{name}

data GuardianFactorUpdate
  = GuardianFactorUpdate
  { enabled :: Bool
  } deriving (Generic, Show)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''GuardianFactorUpdate

runUpdateGuardianFactor
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> GuardianFactorUpdate -> m (Auth0Response GuardianFactorUpdate)
runUpdateGuardianFactor a i o =
  let api = API Put ("/api/v2/guardian/factors/" <> encodeUtf8 i)
  in execRequest a api (Nothing :: Maybe ()) (Just o) Nothing
