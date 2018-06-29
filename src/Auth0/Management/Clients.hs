{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Clients where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Map
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/clients

-- Request

data Client
  = Client
  { fields        :: Maybe Text
  , includeFields :: Maybe Bool
  , page          :: Maybe Int
  , perPage       :: Maybe Int
  , includeTotals :: Maybe Bool
  } deriving (Show)

instance ToRequest Client where
  toRequest (Client a b c d e) =
    [ toField "fields" a
    , toField "include_fields" b
    , toField "page" c
    , toField "per_page" d
    , toField "include_totals" e
    ]

-- Response

data JwtConfiguration
  = JwtConfiguration
  { lifetimeInSeconds :: Maybe Int
  , secretEncoded     :: Maybe Bool
  , scopes            :: Maybe (Map Text Text)
  , alg               :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON JwtConfiguration where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON JwtConfiguration where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data EncryptionKey
  = EncryptionKey
  { pub     :: Maybe Text
  , cert    :: Maybe Text
  , subject :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON EncryptionKey where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON EncryptionKey where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data Android
  = Android
  { appPackageName         :: Text
  , sha256CertFingerprints :: [Text]
  } deriving (Generic, Show)

instance FromJSON Android where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Android where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data IOS
  = IOS
  { teamId              :: Text
  , appBundleIdentifier :: Text
  } deriving (Generic, Show)

instance FromJSON IOS where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON IOS where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data Mobile
  = Mobile
  { android :: Maybe Android
  , ios     :: Maybe IOS
  } deriving (Generic, Show)

instance FromJSON Mobile
instance ToJSON Mobile

data ClientResponse
  = ClientResponse
  { name                    :: Maybe Text
  , description             :: Maybe Text
  , clientId                :: Maybe ClientId
  , clientSecret            :: Maybe ClientSecret
  , appType                 :: Maybe Text
  , logoUri                 :: Maybe Text
  , isFirstParty            :: Bool
  , oidcConformant          :: Bool
  , callbacks               :: Maybe [Text]
  , allowedOrigins          :: Maybe [Text]
  , webOrigins              :: Maybe [Text]
  , clientAliases           :: Maybe [Text]
  , allowedClients          :: Maybe [Text]
  , allowedLoginUrls        :: Maybe [Text]
  , jwtConfiguration        :: Maybe JwtConfiguration
  , signingKeys             :: Maybe [Text]
  , encryptionKey           :: Maybe EncryptionKey
  , sso                     :: Maybe Bool
  , ssoDisabled             :: Maybe Bool
  , crossOriginAuth         :: Maybe Bool
  , crossOriginLoc          :: Maybe Text
  , customLoginPageOn       :: Maybe Bool
  , customLoginPage         :: Maybe Text
  , customLoginPagePreview  :: Maybe Text
  , formTemplate            :: Maybe Text
  , addons                  :: Maybe (Map Text Text)
  , tokenEndpointAuthMethod :: Maybe Text
  , clientMetadata          :: Maybe (Map Text Text)
  , mobile                  :: Maybe Mobile
  } deriving (Generic, Show)

instance FromJSON ClientResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetClients
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Maybe Client -> m (Auth0Response [ClientResponse])
runGetClients (TokenAuth tenant accessToken) o =
  let api = API Get "/api/v2/clients"
  in execRequest tenant api o (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/clients

data ClientCreate
  = ClientCreate
  { name                    :: Maybe Text
  , description             :: Maybe Text
  , clientId                :: Maybe ClientId
  , clientSecret            :: Maybe ClientSecret
  , logoUri                 :: Maybe Text
  , callbacks               :: Maybe [Text]
  , allowedOrigins          :: Maybe [Text]
  , webOrigins              :: Maybe [Text]
  , clientAliases           :: Maybe [Text]
  , allowedClients          :: Maybe [Text]
  , allowedLogoutUrls       :: Maybe [Text]
  , grantTypes              :: Maybe [Text]
  , tokenEndpointAuthMethod :: Maybe Text
  , oidcConformant          :: Bool
  , jwtConfiguration        :: Maybe JwtConfiguration
  , encryptionKey           :: Maybe EncryptionKey
  , appType                 :: Maybe Text
  , sso                     :: Maybe Bool
  , crossOriginAuth         :: Maybe Bool
  , crossOriginLoc          :: Maybe Text
  , ssoDisabled             :: Maybe Bool
  , customLoginPageOn       :: Maybe Bool
  , customLoginPage         :: Maybe Text
  , customLoginPagePreview  :: Maybe Text
  , formTemplate            :: Maybe Text
  , isHerokuApp             :: Maybe Bool
  , addons                  :: Maybe (Map Text Text)
  , clientMetadata          :: Maybe (Map Text Text)
  , mobile                  :: Maybe Mobile
  } deriving (Generic, Show)

instance ToJSON ClientCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateClient
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> ClientCreate -> m (Auth0Response ClientResponse)
runCreateClient (TokenAuth tenant accessToken) o =
  let api = API Post "/api/v2/clients"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- GET /api/v2/clients/{id}

data ClientGet
  = ClientGet
  { id            :: Text
  , fields        :: Maybe Text
  , includeFields :: Maybe Bool
  }

instance ToRequest ClientGet where
  toRequest (ClientGet a b c) =
    [ toField "id" a
    , toField "fields" b
    , toField "include_fields" c
    ]

runGetClient
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> ClientGet -> m (Auth0Response ClientResponse)
runGetClient (TokenAuth tenant accessToken) i o =
  let api = API Get ("/api/v2/clients/" <> encodeUtf8 i)
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- DELETE /api/v2/clients/{id}

runDeleteClient
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response ())
runDeleteClient (TokenAuth tenant accessToken) i =
  let api = API Delete ("/api/v2/clients/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- PATCH /api/v2/clients/{id}

type ClientUpdate = ClientCreate

runUpdateClient
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> ClientUpdate -> m (Auth0Response ())
runUpdateClient (TokenAuth tenant accessToken) i o =
  let api = API Update ("/api/v2/clients/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/clients/{id}/rotate-secret

runClientRotateSecret
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response ClientResponse)
runClientRotateSecret (TokenAuth tenant accessToken) i =
  let api = API Update ("/api/v2/clients/" <> encodeUtf8 i <> "/rotate-secret")
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])
