{-# LANGUAGE DeriveGeneric #-}

module Auth0.Types where

---------------------------------------------------------------------------------
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Tagged
import GHC.Generics
---------------------------------------------------------------------------------

data ClientIdTag
type ClientId = Tagged ClientIdTag Text

mkClientId :: Text -> ClientId
mkClientId = Tagged

data ClientSecretTag
type ClientSecret = Tagged ClientSecretTag Text

mkClientSecret :: Text -> ClientSecret
mkClientSecret = Tagged

data AccessTokenTag
type AccessToken = Tagged AccessTokenTag Text

mkAccessToken :: Text -> AccessToken
mkAccessToken = Tagged

data UserIdTag
type UserId = Tagged UserIdTag Text

mkUserId :: Text -> UserId
mkUserId = Tagged

---------------------------------------------------------------------------------

-- | The Auth0 provided client id and secret key.
data Credentials
  = Credentials
  { clientId     :: ClientId
  , clientSecret :: ClientSecret
  }

-- | A potential JSON error any API can throw.
data Auth0Error
  = Auth0Error
  { error            :: Text
  , errorDescription :: Text
  } deriving (Generic)

instance ToJSON Auth0Error where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Auth0Error where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

---------------------------------------------------------------------------------

data ResponseType
  = Code
  | Token

instance ToJSON ResponseType where
  toJSON Code  = "code"
  toJSON Token = "token"

data GrantType
  = Password
  | AuthorizationCode
  | ClientCredentials
  | OTP

instance ToJSON GrantType where
  toJSON Password          = "password"
  toJSON AuthorizationCode = "authorization_code"
  toJSON ClientCredentials = "client_credentials"
  toJSON OTP               = "http://auth0.com/oauth/grant-type/mfa-otp"
