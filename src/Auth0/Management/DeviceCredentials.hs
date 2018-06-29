{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.DeviceCredentials where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/device-credentials

-- Request

data DeviceCredential
  = DeviceCredential
  { fields        :: Maybe Text
  , includeFields :: Maybe Bool
  , userId        :: Maybe Text
  , clientId      :: Maybe ClientId
  , ctype         :: Maybe Text
  } deriving (Show)

instance ToRequest DeviceCredential where
  toRequest (DeviceCredential a b c d e) =
    [ toField "fields" a
    , toField "include_fields" b
    , toField "user_id" c
    , toField "client_id" d
    , toField "type" e
    ]

-- Response

data DeviceCredentialResponse
  = DeviceCredentialResponse
  { id         :: Text
  , deviceName :: Text
  , deviceId   :: Text
  , ctype      :: Text
  , userId     :: Text
  } deriving (Generic, Show)

instance FromJSON DeviceCredentialResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = f }
    where
      f "type" = "ctype"
      f v      = camelTo2 '_' v

runGetDeviceCredentials
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> DeviceCredential -> m (Auth0Response [DeviceCredentialResponse])
runGetDeviceCredentials (TokenAuth tenant accessToken) o =
  let api = API Get "/api/v2/device-credentials"
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/device-credentials

data DeviceCredentialCreate
  = DeviceCredentialCreate
  { deviceName :: Text
  , ctype      :: Text
  , value      :: Text
  , deviceId   :: Text
  , clientId   :: ClientId
  } deriving (Generic, Show)

instance ToJSON DeviceCredentialCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = f }
    where
      f "ctype" = "type"
      f v       = camelTo2 '_' v

data DeviceCredentialId
  = DeviceCredentialId
  { id :: Text
  } deriving (Generic, Show)

instance FromJSON DeviceCredentialId

runCreateDeviceCredential
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> DeviceCredentialCreate -> m (Auth0Response DeviceCredentialId)
runCreateDeviceCredential (TokenAuth tenant accessToken) o =
  let api = API Post "/api/v2/device-credentials"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- DELETE /api/v2/device-credentials/{id}

runDeleteDeviceCredential
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response DeviceCredentialId)
runDeleteDeviceCredential (TokenAuth tenant accessToken) i =
  let api = API Delete ("/api/v2/device-credentials/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])
