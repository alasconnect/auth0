{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.ResourceServers where

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
-- GET /api/v2/resource-servers

-- Response

data ResourceServerResponse
  = ResourceServerResponse
  { identifier          :: Maybe Text
  , scopes              :: Maybe [Text]
  , signingAlg          :: Maybe Text
  , signingSecret       :: Maybe Text
  , allowOfflineAccess  :: Maybe Bool
  , skipConsentForVerifiableFirstPartyClients :: Maybe Bool
  , tokenLifetime       :: Maybe Int
  , tokenLifetimeForWeb :: Maybe Int
  , id                  :: Maybe Text
  , name                :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON ResourceServerResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetResourceServers
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> m (Auth0Response [ResourceServerResponse])
runGetResourceServers (TokenAuth tenant accessToken) =
  let api = API Get "/api/v2/resource-servers"
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/resource-servers

data ResourceServerCreate
  = ResourceServerCreate
  { name                 :: Maybe Text
  , identifier           :: Maybe Text
  , scopes               :: Maybe [Text]
  , signingAlg           :: Maybe Text
  , signingSecret        :: Maybe Text
  , allowOfflineAccess   :: Maybe Bool
  , skipConsentForVerifiableFirstPartyClients :: Maybe Bool
  , verificationKey      :: Maybe Text
  , verificationLocation :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON ResourceServerCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateResourceServer
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> ResourceServerCreate -> m (Auth0Response [ResourceServerResponse])
runCreateResourceServer (TokenAuth tenant accessToken) o =
  let api = API Post "/api/v2/resource-servers"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- GET /api/v2/resource-servers/{id}

runGetResourceServer
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response ResourceServerResponse)
runGetResourceServer (TokenAuth tenant accessToken) i =
  let api = API Get ("/api/v2/resource-servers/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- DELETE /api/v2/resource-servers/{id}

runDeleteResourceServer
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response ResourceServerResponse)
runDeleteResourceServer (TokenAuth tenant accessToken) i =
  let api = API Delete ("/api/v2/resource-servers/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- PATCH /api/v2/resource-servers/{id}

type ResourceServerUpdate = ResourceServerCreate

runUpdateResourceServer
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> ResourceServerUpdate -> m (Auth0Response ResourceServerResponse)
runUpdateResourceServer (TokenAuth tenant accessToken) i o =
  let api = API Update ("/api/v2/resource-servers/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])
