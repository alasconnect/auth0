{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Connections where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson hiding (Options)
import Data.Map
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
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
  } deriving (Show)

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
  } deriving (Generic, Show)

instance FromJSON ConnectionResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetConnections
  :: (MonadIO m, MonadThrow m)
  => Auth -> Connection -> m (Auth0Response [ConnectionResponse])
runGetConnections a o =
  let api = API Get "/api/v2/connections"
  in execRequest a api (Just o) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
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
  } deriving (Generic, Show)

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
  } deriving (Generic, Show)

instance ToJSON ConnectionCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateConnection
  :: (MonadIO m, MonadThrow m)
  => Auth -> ConnectionCreate -> m (Auth0Response ConnectionResponse)
runCreateConnection a o =
  let api = API Post "/api/v2/connections"
  in execRequest a api (Nothing :: Maybe ()) (Just o) Nothing

--------------------------------------------------------------------------------
-- GET /api/v2/connections/{id}

data ConnectionGet
  = ConnectionGet
  { fields        :: Maybe Text
  , includeFields :: Maybe Bool
  } deriving (Show)

instance ToRequest ConnectionGet where
  toRequest (ConnectionGet a b) =
    [ toField "fields" a
    , toField "include_fields" b
    ]

runGetConnection
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> ConnectionGet -> m (Auth0Response ConnectionResponse)
runGetConnection a i o =
  let api = API Get ("/api/v2/connections/" <> encodeUtf8 i)
  in execRequest a api (Just o) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- DELETE /api/v2/connections/{id}

runDeleteConnection
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> m (Auth0Response ())
runDeleteConnection a i =
  let api = API Delete ("/api/v2/connections/" <> encodeUtf8 i)
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- PATCH /api/v2/connections/{id}

data ConnectionUpdate
  = ConnectionUpdate
  { options        :: Maybe Options
  , enabledClients :: Maybe (Map Text Text)
  , realms         :: Maybe (Map Text Text)
  , metadata       :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON ConnectionUpdate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runUpdateConnection
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> m (Auth0Response ConnectionResponse)
runUpdateConnection a i =
  let api = API Update ("/api/v2/connections/" <> encodeUtf8 i)
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- DELETE /api/v2/connections/{id}/users

data ConnectionUserDelete
  = ConnectionUserDelete
  { email :: Maybe Text
  } deriving (Show)

instance ToRequest ConnectionUserDelete where
  toRequest (ConnectionUserDelete a) =
    [ toField "email" a ]

runDeleteConnectionUser
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> ConnectionUserDelete -> m (Auth0Response ConnectionResponse)
runDeleteConnectionUser a i o =
  let api = API Update ("/api/v2/connections/" <> encodeUtf8 i)
  in execRequest a api (Just o) (Nothing :: Maybe ()) Nothing
