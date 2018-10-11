{-# LANGUAGE FlexibleInstances #-}

module Auth0.Types where
  
--------------------------------------------------------------------------------
import Data.Aeson
import Data.ByteString (ByteString)
import Servant.API hiding (Verb)
import Data.Text
import Data.Tagged
--------------------------------------------------------------------------------

data TenantTag
type Tenant = Tagged TenantTag ByteString

mkTenant :: ByteString -> Tenant
mkTenant = Tagged

data AccessTokenTag
type AccessToken = Tagged AccessTokenTag Text

mkAccessToken :: Text -> AccessToken
mkAccessToken = Tagged

instance ToHttpApiData AccessToken where
  toUrlPiece = untag

data ClientIdTag
type ClientId = Tagged ClientIdTag Text

mkClientId :: Text -> ClientId
mkClientId = Tagged

instance ToHttpApiData ClientId where
  toUrlPiece = untag

data ClientSecretTag
type ClientSecret = Tagged ClientSecretTag Text

mkClientSecret :: Text -> ClientSecret
mkClientSecret = Tagged

data ResponseType
  = Code
  | Token
  deriving (Show)

instance ToHttpApiData ResponseType where
  toUrlPiece Code  = "code"
  toUrlPiece Token = "token"

instance ToJSON ResponseType where
  toJSON Code  = "code"
  toJSON Token = "token"

data GrantType
  = Password
  | AuthorizationCode
  | ClientCredentials
  | OTP
  deriving (Show)

instance ToJSON GrantType where
  toJSON Password          = "password"
  toJSON AuthorizationCode = "authorization_code"
  toJSON ClientCredentials = "client_credentials"
  toJSON OTP               = "http://auth0.com/oauth/grant-type/mfa-otp"
