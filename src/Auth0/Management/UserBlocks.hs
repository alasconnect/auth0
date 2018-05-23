{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.UserBlocks where

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
-- GET /api/v2/user-blocks

-- Request

data UserBlock
  = UserBlock
  { identifier :: Text
  } deriving (Show)

instance ToRequest UserBlock where
  toRequest (UserBlock a) =
    [ toField "identifier" a
    ]

-- Response

data BlockedFor
  = BlockedFor
  { identifier :: Text
  , ip         :: Text
  } deriving (Generic, Show)

instance FromJSON BlockedFor

data UserBlockResponse
  = UserBlockResponse
  { blockedFor :: Maybe [BlockedFor]
  } deriving (Generic, Show)

instance FromJSON UserBlockResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetUserBlocks
  :: (MonadIO m, MonadThrow m)
  => Auth -> UserBlock -> m (Auth0Response BlockedFor)
runGetUserBlocks a o =
  let api = API Get "/api/v2/user-blocks"
  in execRequest a api (Just o) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- DELETE /api/v2/user-blocks

runDeleteUserBlock
  :: (MonadIO m, MonadThrow m)
  => Auth -> UserBlock -> m (Auth0Response ())
runDeleteUserBlock a o =
  let api = API Delete "/api/v2/user-blocks"
  in execRequest a api (Just o) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- GET /api/v2/user-blocks/{id}

runGetUserBlock
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> m (Auth0Response BlockedFor)
runGetUserBlock a i =
  let api = API Get ("/api/v2/user-blocks/" <> encodeUtf8 i)
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

--------------------------------------------------------------------------------
-- DELETE /api/v2/user-blocks/{id}

runUnblockUserBlock
  :: (MonadIO m, MonadThrow m)
  => Auth -> Text -> m (Auth0Response BlockedFor)
runUnblockUserBlock a i =
  let api = API Delete ("/api/v2/user-blocks/" <> encodeUtf8 i)
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing
