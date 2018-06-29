{-# LANGUAGE DeriveGeneric #-}

module Auth0.Authentication.Impersonation where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.ByteString
import Data.Map
import Data.Monoid ((<>))
import Data.Text
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

data Protocol
  = OAuth2
  | SAMLP
  | WSFed
  | WSFedRMS
  deriving (Generic)

instance Show Protocol where
  show OAuth2   = "oauth2"
  show SAMLP    = "samlp"
  show WSFed    = "wsfed"
  show WSFedRMS = "wsfed-rms"

instance ToJSON Protocol

-- POST /users/{user_id}/impersonate

data Impersonate
  = Impersonate
  { protocol             :: Protocol
  , impersonatorId       :: Text
  , clientId             :: ClientId
  , additionalParameters :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON Impersonate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runImpersonate
  :: (MonadIO m, MonadThrow m)
  => Auth -> ByteString -> Impersonate -> m (Auth0Response Text)
runImpersonate (Auth tenant) uid o =
  let api = API Post ("/users/" <> uid <> "/impersonate")
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) Nothing
