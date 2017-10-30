{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.ResourceServers where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
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
  } deriving (Generic)

instance FromJSON ResourceServerResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetResourceServers
  :: (MonadIO m, MonadThrow m)
  => Host -> m (Int, Maybe [ResourceServerResponse])
runGetResourceServers h =
  let api = API Get "/api/v2/resource-servers"
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
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
  } deriving (Generic)

instance ToJSON ResourceServerCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateResourceServer
  :: (MonadIO m, MonadThrow m)
  => Host -> ResourceServerCreate -> m (Int, Maybe [ResourceServerResponse])
runCreateResourceServer h o =
  let api = API Post "/api/v2/resource-servers"
  in execRequest h api () o Nothing

---------------------------------------------------------------------------------
-- GET /api/v2/resource-servers/{id}

runGetResourceServer
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe ResourceServerResponse)
runGetResourceServer h i =
  let api = API Get ("/api/v2/resource-servers/" <> encodeUtf8 i)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/resource-servers/{id}

runDeleteResourceServer
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe ResourceServerResponse)
runDeleteResourceServer h i =
  let api = API Delete ("/api/v2/resource-servers/" <> encodeUtf8 i)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- PATCH /api/v2/resource-servers/{id}

type ResourceServerUpdate = ResourceServerCreate

runUpdateResourceServer
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> ResourceServerUpdate -> m (Int, Maybe ResourceServerResponse)
runUpdateResourceServer h i o =
  let api = API Update ("/api/v2/resource-servers/" <> encodeUtf8 i)
  in execRequest h api () o Nothing
