{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.ClientGrants where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Map
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- GET /api/v2/client-grants

data ClientGrant
  = ClientGrant
  { id       :: Maybe Text
  , clientId :: Maybe ClientId
  , audience :: Maybe Text
  , scope    :: Maybe (Map Text Text)
  } deriving (Generic)

instance FromJSON ClientGrant where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetClientGrants
  :: (MonadIO m, MonadThrow m)
  => Host -> m (Int, Maybe [ClientGrant])
runGetClientGrants h =
  let api = API Get "/api/v2/client-grants"
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- POST /api/v2/client-grants

data ClientGrantCreate
  = ClientGrantsreate
  { clientId :: ClientId
  , audience :: Text
  , scope    :: (Map Text Text)
  } deriving (Generic)

instance ToJSON ClientGrantCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateClientGrant
  :: (MonadIO m, MonadThrow m)
  => Host -> ClientGrantCreate -> m (Int, Maybe ())
runCreateClientGrant h o =
  let api = API Post "/api/v2/client-grants"
  in execRequest h api () o Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/client-grants/{id}

runDeleteClientGrant
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe ())
runDeleteClientGrant h i =
  let api = API Delete ("/api/v2/client-grants/" <> encodeUtf8 i)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- PATCH /api/v2/client-grants/{id}

data ClientGrantUpdate
  = ClientGrantUpdate
  { scope    :: Maybe (Map Text Text)
  } deriving (Generic)

instance ToJSON ClientGrantUpdate

runUpdateClientGrant
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> ClientGrantUpdate -> m (Int, Maybe ClientGrant)
runUpdateClientGrant h i o =
  let api = API Update ("/api/v2/client-grants/" <> encodeUtf8 i)
  in execRequest h api () o Nothing
