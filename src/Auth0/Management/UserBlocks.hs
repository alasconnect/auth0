{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.UserBlocks where

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
-- GET /api/v2/user-blocks

-- Request

data UserBlock
  = UserBlock
  { identifier :: Text
  }

instance ToRequest UserBlock where
  toRequest (UserBlock a) =
    [ toField "identifier" a
    ]

-- Response

data BlockedFor
  = BlockedFor
  { identifier :: Text
  , ip         :: Text
  } deriving (Generic)

instance FromJSON BlockedFor

data UserBlockResponse
  = UserBlockResponse
  { blockedFor :: Maybe [BlockedFor]
  } deriving (Generic)

instance FromJSON UserBlockResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetUserBlocks
  :: (MonadIO m, MonadThrow m)
  => Host -> UserBlock -> m (Int, Maybe BlockedFor)
runGetUserBlocks h o =
  let api = API Get "/api/v2/user-blocks"
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/user-blocks

runDeleteUserBlock
  :: (MonadIO m, MonadThrow m)
  => Host -> UserBlock -> m (Int, Maybe ())
runDeleteUserBlock h o =
  let api = API Delete "/api/v2/user-blocks"
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- GET /api/v2/user-blocks/{id}

runGetUserBlock
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe BlockedFor)
runGetUserBlock h i =
  let api = API Get ("/api/v2/user-blocks/" <> encodeUtf8 i)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/user-blocks/{id}

runUnblockUserBlock
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe BlockedFor)
runUnblockUserBlock h i =
  let api = API Delete ("/api/v2/user-blocks/" <> encodeUtf8 i)
  in execRequest h api () () Nothing
