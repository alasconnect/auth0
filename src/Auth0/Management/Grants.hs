{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Grants where

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
-- GET /api/v2/grants

-- Request

data Grant
  = Grant
  { userId :: Text
  } deriving (Show)

instance ToRequest Grant where
  toRequest (Grant a) =
    [ toField "user_id" a ]

-- Response

data GrantResponse
  = GrantResponse
  { id       :: Text
  , clientId :: Text
  , userId   :: Text
  , audience :: Text
  , scope    :: [Text]
  } deriving (Generic, Show)

instance FromJSON GrantResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetGrants
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Grant -> m (Auth0Response [GrantResponse])
runGetGrants (TokenAuth tenant accessToken) o =
  let api = API Get "/api/v2/grants"
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- DELETE /api/v2/grants/{id}

runDeleteGrant
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> Grant -> m (Auth0Response ())
runDeleteGrant (TokenAuth tenant accessToken) i o =
  let api = API Delete ("/api/v2/grants/" <> encodeUtf8 i)
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])
