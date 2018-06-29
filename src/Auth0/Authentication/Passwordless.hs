{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Authentication.Passwordless where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Text
import Data.Map
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

-- POST /passwordless/start

data ConnectionType
  = TEmail
  | TSms
  deriving (Show)

instance ToJSON ConnectionType where
  toJSON TEmail = "email"
  toJSON TSms   = "sms"

data SendType
  = TLink
  | TCode
  deriving (Show)

instance ToJSON SendType where
  toJSON TLink = "link"
  toJSON TCode = "code"

data GetCodeOrLink
  = GetCodeOrLink
  { clientId    :: ClientId
  , connection  :: ConnectionType
  , email       :: Maybe Text
  , phoneNumber :: Maybe Text
  , send        :: Maybe SendType
  , authParams  :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON GetCodeOrLink where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetCodeOrLink
  :: (MonadIO m, MonadThrow m)
  => Auth -> GetCodeOrLink -> m (Auth0Response ())
runGetCodeOrLink (Auth tenant) o =
  let api = API Post "/passwordless/start"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) Nothing

-- POST /oauth/ro

data AuthenticateUser
  = AuthenticateUser
  { clientId   :: ClientId
  , connection :: ConnectionType
  , grantType  :: GrantType
  , username   :: Text
  , password   :: Text           -- ^ User's verification code
  , scope      :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON AuthenticateUser where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runAuthenticateUser
  :: (MonadIO m, MonadThrow m)
  => Auth -> AuthenticateUser -> m (Auth0Response ())
runAuthenticateUser (Auth tenant) o =
  let api = API Post "/oauth/ro"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) Nothing
