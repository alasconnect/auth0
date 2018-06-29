{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.ClientGrants where

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
-- GET /api/v2/client-grants

-- Request

data ClientGrant
  = ClientGrant
  { audience :: Maybe Text
  , clientId :: Maybe ClientId
  } deriving (Show)

instance ToRequest ClientGrant where
  toRequest (ClientGrant a b) =
    [ toField "audience" a
    , toField "client_id" b
    ]

-- Response

data ClientGrantResponse
  = ClientGrantResponse
  { id       :: Maybe Text
  , clientId :: Maybe ClientId
  , audience :: Maybe Text
  , scope    :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance FromJSON ClientGrantResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetClientGrants
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Maybe ClientGrant -> m (Auth0Response [ClientGrantResponse])
runGetClientGrants (TokenAuth tenant accessToken) o =
  let api = API Get "/api/v2/client-grants"
  in execRequest tenant api o (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/client-grants

data ClientGrantCreate
  = ClientGrantsreate
  { clientId :: ClientId
  , audience :: Text
  , scope    :: (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON ClientGrantCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateClientGrant
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> ClientGrantCreate -> m (Auth0Response ())
runCreateClientGrant (TokenAuth tenant accessToken) o =
  let api = API Post "/api/v2/client-grants"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- DELETE /api/v2/client-grants/{id}

runDeleteClientGrant
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response ())
runDeleteClientGrant (TokenAuth tenant accessToken) i =
  let api = API Delete ("/api/v2/client-grants/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- PATCH /api/v2/client-grants/{id}

data ClientGrantUpdate
  = ClientGrantUpdate
  { scope    :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON ClientGrantUpdate

runUpdateClientGrant
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> ClientGrantUpdate -> m (Auth0Response ClientGrantResponse)
runUpdateClientGrant (TokenAuth tenant accessToken) i o =
  let api = API Update ("/api/v2/client-grants/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])
