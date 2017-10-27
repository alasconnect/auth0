{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Authentication.AccountLinking where

---------------------------------------------------------------------------------
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
---------------------------------------------------------------------------------

-- POST /api/v2/users/{id}/identities

data AccountLink
  = AccountLink
  { responseType :: ResponseType
  , clientId     :: ClientId
  , connection   :: Maybe Text
  , redirectUri  :: Text
  , accessToken  :: Text
  }

-- DELETE /api/v2/users/{id}/identities/{provider}/{user_id}

data AccountUnlink
  = AccountUnlink
  { accessToken :: Text
  , userId      :: Text
  } deriving (Generic)

instance ToJSON AccountUnlink where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
