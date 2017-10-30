{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Connections where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types hiding (Options)
import Data.Map
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- GET /api/v2/connections

-- Request

data Connection
  = Connection
  { perPage       :: Int
  , page          :: Int
  , strategy      :: Map Text Text
  , name          :: Text
  , fields        :: Text
  , includeFields :: Bool
  }

instance ToRequest Connection where
  toRequest (Connection a b c d e f) =
    [ toField "per_page" a
    , toField "page" b
    ] ++ flattenMap "strategy" c ++
    [ toField "name" d
    , toField "fields" e
    , toField "include_fields" f
    ]

-- Response

data ConnectionResponse
  = ConnectionResponse
  { name               :: Maybe Text
  , options            :: Maybe (Map Text Text)
  , id                 :: Maybe Text
  , strategy           :: Maybe Text
  , realms             :: Maybe (Map Text Text)
  , enabledClients     :: Maybe (Map Text Text)
  , isDomainConnection :: Maybe Bool
  , metadata           :: Maybe (Map Text Text)
  } deriving (Generic)

instance FromJSON ConnectionResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetConnections
  :: (MonadIO m, MonadThrow m)
  => Host -> Connection -> m (Int, Maybe [ConnectionResponse])
runGetConnections h o =
  let api = API Get "/api/v2/connections"
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- POST /api/v2/connections

-- Request

data Options
  = Options
  { validation             :: Maybe Text
  , passwordPolicy         :: Maybe Text
  , passwordHistory        :: Maybe Text
  , passwordNoPersonalInfo :: Maybe Text
  , passwordDictionary     :: Maybe Text
  , upstreamParams         :: Maybe Text
  } deriving (Generic)

instance ToJSON Options where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data ConnectionCreate
  = ConnectionCreate
  { name           :: Text
  , strategy       :: Text
  , options        :: Maybe Options
  , enabledClients :: Maybe (Map Text Text)
  , realms         :: Maybe (Map Text Text)
  , metadata       :: Maybe (Map Text Text)
  } deriving (Generic)

instance ToJSON ConnectionCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateConnection
  :: (MonadIO m, MonadThrow m)
  => Host -> ConnectionCreate -> m (Int, Maybe ConnectionResponse)
runCreateConnection h o =
  let api = API Post "/api/v2/connections"
  in execRequest h api () o Nothing

---------------------------------------------------------------------------------
-- GET /api/v2/connections/{id}

data ConnectionGet
  = ConnectionGet
  { fields        :: Maybe Text
  , includeFields :: Maybe Bool
  }

instance ToRequest ConnectionGet where
  toRequest (ConnectionGet a b) =
    [ toField "fields" a
    , toField "include_fields" b
    ]

runGetConnection
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> ConnectionGet -> m (Int, Maybe ConnectionResponse)
runGetConnection h i o =
  let api = API Get ("/api/v2/connections/" <> encodeUtf8 i)
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/connections/{id}

runDeleteConnection
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe ())
runDeleteConnection h i =
  let api = API Delete ("/api/v2/connections/" <> encodeUtf8 i)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- PATCH /api/v2/connections/{id}

data ConnectionUpdate
  = ConnectionUpdate
  { options        :: Maybe Options
  , enabledClients :: Maybe (Map Text Text)
  , realms         :: Maybe (Map Text Text)
  , metadata       :: Maybe (Map Text Text)
  } deriving (Generic)

instance ToJSON ConnectionUpdate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runUpdateConnection
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe ConnectionResponse)
runUpdateConnection h i =
  let api = API Update ("/api/v2/connections/" <> encodeUtf8 i)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/connections/{id}/users

data ConnectionUserDelete
  = ConnectionUserDelete
  { email :: Maybe Text
  }

instance ToRequest ConnectionUserDelete where
  toRequest (ConnectionUserDelete a) =
    [ toField "email" a ]

runDeleteConnectionUser
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> ConnectionUserDelete -> m (Int, Maybe ConnectionResponse)
runDeleteConnectionUser h i o =
  let api = API Update ("/api/v2/connections/" <> encodeUtf8 i)
  in execRequest h api o () Nothing
