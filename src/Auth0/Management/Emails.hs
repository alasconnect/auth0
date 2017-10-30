{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Emails where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Map
import Data.Text
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- GET /api/v2/emails/provider

-- Request

data EmailProvider
  = EmailProvider
  { fields        :: Maybe Text
  , includeFields :: Maybe Bool
  }

instance ToRequest EmailProvider where
  toRequest (EmailProvider a b) =
    [ toField "fields" a
    , toField "include_fields" b
    ]

-- Response

data Credentials
  = Credentials
  { apiUser         :: Maybe Text
  , apiKey          :: Maybe Text
  , accessKeyId     :: Maybe Text
  , secretAccessKey :: Maybe Text
  , region          :: Maybe Text
  , smtpHost        :: Maybe Text
  , smtpPort        :: Maybe Int
  , smtpUser        :: Maybe Text
  , smtpPass        :: Maybe Text
  } deriving (Generic)

instance FromJSON Credentials where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data EmailProviderResponse
  = EmailProviderResponse
  { name               :: Maybe Text
  , enabled            :: Maybe Bool
  , defaultFromAddress :: Maybe Text
  , credentials        :: Maybe Credentials
  , settings           :: Maybe (Map Text Text)
  } deriving (Generic)

instance FromJSON EmailProviderResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetEmailProviders
  :: (MonadIO m, MonadThrow m)
  => Host -> EmailProvider -> m (Int, Maybe [EmailProviderResponse])
runGetEmailProviders h o =
  let api = API Get "/api/v2/emails/provider"
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/emails/provider

runDeleteEmailProvider
  :: (MonadIO m, MonadThrow m)
  => Host -> m (Int, Maybe ())
runDeleteEmailProvider h =
  let api = API Delete "/api/v2/emails/provider"
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- PATCH /api/v2/emails/provider

data CredentialsUpdate
  = CredentialsUpdate
  { apiKey :: Text
  } deriving (Generic)

instance ToJSON CredentialsUpdate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data EmailProviderUpdate
  = EmailProviderUpdate
  { name               :: Maybe Text
  , enabled            :: Maybe Bool
  , defaultFromAddress :: Maybe Text
  , credentials        :: Maybe CredentialsUpdate
  , settings           :: Maybe (Map Text Text)
  } deriving (Generic)

instance ToJSON EmailProviderUpdate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runUpdateEmailProvider
  :: (MonadIO m, MonadThrow m)
  => Host -> EmailProviderUpdate -> m (Int, Maybe EmailProviderResponse)
runUpdateEmailProvider h o =
  let api = API Update "/api/v2/emails/provider"
  in execRequest h api () o Nothing

---------------------------------------------------------------------------------
-- POST /api/v2/emails/provider

type EmailProviderCreate = EmailProviderUpdate

runCreateEmailProvider
  :: (MonadIO m, MonadThrow m)
  => Host -> EmailProviderCreate -> m (Int, Maybe EmailProviderResponse)
runCreateEmailProvider h o =
  let api = API Post "/api/v2/emails/provider"
  in execRequest h api () o Nothing
