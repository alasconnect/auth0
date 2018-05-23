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
  => Auth -> m (Auth0Response [ResourceServerResponse])
runGetResourceServers a =
  let api = API Get "/api/v2/resource-servers"
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

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
  => Auth -> ResourceServerCreate -> m (Auth0Response [ResourceServerResponse])
runCreateResourceServer a o =
  let api = API Post "/api/v2/resource-servers"
  in execRequest a api (Nothing :: Maybe ()) (Just o) Nothing

--------------------------------------------------------------------------------
-- GET /api/v2/resource-servers/{id}

runGetResourceServer
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> m (Auth0Response ResourceServerResponse)
runGetResourceServer a i =
  let api = API Get ("/api/v2/resource-servers/" <> encodeUtf8 i)
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- DELETE /api/v2/resource-servers/{id}

runDeleteResourceServer
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> m (Auth0Response ResourceServerResponse)
runDeleteResourceServer a i =
  let api = API Delete ("/api/v2/resource-servers/" <> encodeUtf8 i)
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- PATCH /api/v2/resource-servers/{id}

type ResourceServerUpdate = ResourceServerCreate

runUpdateResourceServer
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> ResourceServerUpdate -> m (Auth0Response ResourceServerResponse)
runUpdateResourceServer a i o =
  let api = API Update ("/api/v2/resource-servers/" <> encodeUtf8 i)
  in execRequest a api (Nothing :: Maybe ()) (Just o) Nothing
