{-# LANGUAGE DeriveGeneric #-}

module Auth0.Authentication.RevokeRefreshToken where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Text
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
---------------------------------------------------------------------------------

-- POST /oauth/revoke

data RevokeRefreshToken
  = RevokeRefreshToken
  { clientId     :: ClientId
  , clientSecret :: ClientSecret
  , token        :: Text
  } deriving (Generic)

instance ToJSON RevokeRefreshToken where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runLogin
  :: (MonadIO m, MonadThrow m)
  => ByteString -> RevokeRefreshToken -> m (Int, Maybe ())
runLogin h o =
  let api = API "POST" "/oauth/revoke"
  in execRequest h api () o Nothing
