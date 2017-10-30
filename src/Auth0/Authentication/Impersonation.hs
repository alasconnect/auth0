{-# LANGUAGE DeriveGeneric #-}

module Auth0.Authentication.Impersonation where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Map
import Data.Monoid ((<>))
import Data.Tagged
import Data.Text
import Data.Text.Encoding
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
---------------------------------------------------------------------------------

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
  } deriving (Generic)

instance ToJSON Impersonate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runImpersonate
  :: (MonadIO m, MonadThrow m)
  => Host -> UserId -> Impersonate -> m (Int, Maybe Text)
runImpersonate h uid o =
  let api = API Post ("/users/" <> (encodeUtf8 . untag) uid <> "/impersonate")
      hdr = [("Authorization", "Bearer ")]
  in execRequest h api () o (Just hdr)
