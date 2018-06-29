{-# LANGUAGE DeriveGeneric #-}

module Auth0.Authentication.RevokeRefreshToken where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Text
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

-- POST /oauth/revoke

data RevokeRefreshToken
  = RevokeRefreshToken
  { clientId     :: ClientId
  , clientSecret :: Maybe ClientSecret
  , token        :: Text
  } deriving (Generic, Show)

instance ToJSON RevokeRefreshToken where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runRevokeRefreshToken
  :: (MonadIO m, MonadThrow m)
  => Auth -> RevokeRefreshToken -> m (Auth0Response ())
runRevokeRefreshToken (Auth tenant) o =
  let api = API Post "/oauth/revoke"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) Nothing
