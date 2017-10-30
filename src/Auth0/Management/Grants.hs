{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Grants where

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
-- GET /api/v2/grants

-- Request

data Grant
  = Grant
  { userId :: Text
  }

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
  } deriving (Generic)
  
instance FromJSON GrantResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetGrants
  :: (MonadIO m, MonadThrow m)
  => Host -> Grant -> m (Int, Maybe [GrantResponse])
runGetGrants h o =
  let api = API Get "/api/v2/grants"
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/grants/{id}

runDeleteGrant
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> Grant -> m (Int, Maybe ())
runDeleteGrant h i o =
  let api = API Delete ("/api/v2/grants/" <> encodeUtf8 i)
  in execRequest h api o () Nothing
