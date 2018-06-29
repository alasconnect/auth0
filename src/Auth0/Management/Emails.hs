{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Emails where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Map
import Data.Text
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/emails/provider

-- Request

data EmailProvider
  = EmailProvider
  { fields        :: Maybe Text
  , includeFields :: Maybe Bool
  } deriving (Show)

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
  , smtpAuth        :: Maybe Text
  , smtpPort        :: Maybe Int
  , smtpUser        :: Maybe Text
  , smtpPass        :: Maybe Text
  } deriving (Generic, Show)

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
  } deriving (Generic, Show)

instance FromJSON EmailProviderResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetEmailProviders
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> EmailProvider -> m (Auth0Response [EmailProviderResponse])
runGetEmailProviders (TokenAuth tenant accessToken) o =
  let api = API Get "/api/v2/emails/provider"
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- DELETE /api/v2/emails/provider

runDeleteEmailProvider
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> m (Auth0Response ())
runDeleteEmailProvider (TokenAuth tenant accessToken) =
  let api = API Delete "/api/v2/emails/provider"
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- PATCH /api/v2/emails/provider

data CredentialsUpdate
  = CredentialsUpdate
  { apiKey :: Text
  } deriving (Generic, Show)

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
  } deriving (Generic, Show)

instance ToJSON EmailProviderUpdate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runUpdateEmailProvider
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> EmailProviderUpdate -> m (Auth0Response EmailProviderResponse)
runUpdateEmailProvider (TokenAuth tenant accessToken) o =
  let api = API Update "/api/v2/emails/provider"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/emails/provider

type EmailProviderCreate = EmailProviderUpdate

runCreateEmailProvider
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> EmailProviderCreate -> m (Auth0Response EmailProviderResponse)
runCreateEmailProvider (TokenAuth tenant accessToken) o =
  let api = API Post "/api/v2/emails/provider"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])
