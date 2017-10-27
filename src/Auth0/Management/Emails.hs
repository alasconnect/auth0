{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Emails where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
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
  => Auth -> EmailProvider -> m (Auth0Response [EmailProviderResponse])
runGetEmailProviders a o =
  let api = API Get "/api/v2/emails/provider"
  in execRequest a api (Just o) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- DELETE /api/v2/emails/provider

runDeleteEmailProvider
  :: (MonadIO m, MonadThrow m)
  => Auth -> m (Auth0Response ())
runDeleteEmailProvider a =
  let api = API Delete "/api/v2/emails/provider"
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

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
  => Auth -> EmailProviderUpdate -> m (Auth0Response EmailProviderResponse)
runUpdateEmailProvider a o =
  let api = API Update "/api/v2/emails/provider"
  in execRequest a api (Nothing :: Maybe ()) (Just o) Nothing

--------------------------------------------------------------------------------
-- POST /api/v2/emails/provider

type EmailProviderCreate = EmailProviderUpdate

runCreateEmailProvider
  :: (MonadIO m, MonadThrow m)
  => Auth -> EmailProviderCreate -> m (Auth0Response EmailProviderResponse)
runCreateEmailProvider a o =
  let api = API Post "/api/v2/emails/provider"
  in execRequest a api (Nothing :: Maybe ()) (Just o) Nothing
