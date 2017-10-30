{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Authentication.Passwordless where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Map
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
---------------------------------------------------------------------------------

-- POST /passwordless/start

data ConnectionType
  = TEmail
  | TSms

instance ToJSON ConnectionType where
  toJSON TEmail = "email"
  toJSON TSms   = "sms"

data SendType
  = TLink
  | TCode

instance ToJSON SendType where
  toJSON TLink = "link"
  toJSON TCode = "code"

data GetCodeOrLink
  = GetCodeOrLink
  { clientId    :: ClientId
  , connection  :: ConnectionType
  , send        :: Maybe SendType
  , authParams  :: Maybe (Map Text Text)
  } deriving (Generic)

instance ToJSON GetCodeOrLink where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetCodeOrLink
  :: (MonadIO m, MonadThrow m)
  => Host -> GetCodeOrLink -> m (Int, Maybe ())
runGetCodeOrLink h o =
  let api = API Post "/passwordless/start"
  in execRequest h api () (Just o) Nothing

-- POST /oauth/ro

data AuthenticateUser
  = AuthenticateUser
  { clientId   :: ClientId
  , connection :: ConnectionType
  , grantType  :: GrantType
  , username   :: Text
  , password   :: Text           -- ^ User's verification code
  , scope      :: Maybe Text
  } deriving (Generic)

instance ToJSON AuthenticateUser where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runAuthenticateUser
  :: (MonadIO m, MonadThrow m)
  => Host -> AuthenticateUser -> m (Int, Maybe ())
runAuthenticateUser h o =
  let api = API Post "/oauth/ro"
  in execRequest h api () (Just o) Nothing
