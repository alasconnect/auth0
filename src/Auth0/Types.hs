{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Auth0.Types where

--------------------------------------------------------------------------------
import Control.Monad (mzero)
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Map
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import Data.Tagged
import Network.HTTP.Simple
import Network.HTTP.Types.Header (HeaderName)
--------------------------------------------------------------------------------

-- | 'ToField' describes how to convert a record field to a query string param.
class ToField a where
  toField :: Text -> a -> (Text, Maybe Text)

instance ToField Bool where
  toField t v  = (t, (Just . toLower . pack . show) v)

instance ToField Int where
  toField t v = (t, (Just . pack. show) v)

instance ToField Text where
  toField t v = (t, Just v)

instance ToField a => ToField (Maybe a) where
  toField t Nothing = (t, Nothing)
  toField t (Just v) = toField t v

-- | 'ToRequest' converts a record to a list of tuples.
class ToRequest a where
  toRequest :: a -> [(Text, Maybe Text)]

  buildRequest :: a -> [(ByteString, Maybe ByteString)]
  buildRequest = fmap (bimap encodeUtf8 (fmap encodeUtf8)) . toRequest

instance ToRequest () where
  toRequest _ = [] :: [(Text, Maybe Text)]

instance ToRequest a => ToRequest (Maybe a) where
  toRequest Nothing  = [] :: [(Text, Maybe Text)]
  toRequest (Just a) = toRequest a

--------------------------------------------------------------------------------

data TenantTag
type Tenant = Tagged TenantTag ByteString

mkTenant :: ByteString -> Tenant
mkTenant = Tagged

data AccessTokenTag
type AccessToken = Tagged AccessTokenTag Text

mkAccessToken :: Text -> AccessToken
mkAccessToken = Tagged

data ClientIdTag
type ClientId = Tagged ClientIdTag Text

mkClientId :: Text -> ClientId
mkClientId = Tagged

instance ToField ClientId where
  toField t v = (t, (Just . untag) v)

data ClientSecretTag
type ClientSecret = Tagged ClientSecretTag Text

mkClientSecret :: Text -> ClientSecret
mkClientSecret = Tagged

data ResponseType
  = Code
  | Token
  deriving (Show)

instance ToJSON ResponseType where
  toJSON Code  = "code"
  toJSON Token = "token"

instance ToField ResponseType where
  toField t v = (t, (Just $ case v of
                        Code -> "code"
                        Token -> "token"
                    ))

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

-- | Describes the auth0 hostname to use and is passed to `execRequest`.
data Auth = Auth Tenant deriving (Show)

mkAuth :: ByteString -> Auth
mkAuth = Auth . mkTenant

data TokenAuth = TokenAuth Tenant AccessToken deriving (Show)

mkTokenAuth :: ByteString -> AccessToken -> TokenAuth
mkTokenAuth bs at = TokenAuth (mkTenant bs) at

mkAuthHeader :: AccessToken -> (HeaderName, ByteString)
mkAuthHeader accessToken =
  ("Authorization", encodeUtf8 $ "Bearer " <> untag accessToken)

data Verb
  = Get
  | Post
  | Put
  | Update
  | Delete

instance Show Verb where
  show Get    = "GET"
  show Post   = "POST"
  show Put    = "PUT"
  show Update = "PATCH"
  show Delete = "DELETE"

data API = API Verb ByteString

-- | Describes the potential error data returned.
data Auth0Error
  = Auth0Error
  { auth0StatusCode       :: Int
  , auth0Error            :: Maybe Text
  , auth0Message          :: Maybe Text
  , auth0Name             :: Maybe Text
  , auth0Code             :: Maybe Text
  , auth0ErrorDescription :: Maybe Text
  } deriving (Eq, Show)

instance FromJSON Auth0Error where
  parseJSON (Object v) = Auth0Error
    <$> v .:  "statusCode"
    <*> v .:? "error"
    <*> v .:? "message"
    <*> v .:? "name"
    <*> v .:? "code"
    <*> v .:? "error_description"
  parseJSON _ = mzero

-- | The response from any API call via `execRequest`.
data Auth0Response a
  = Auth0Response
  { resStatusCode :: Int                       -- ^ Response HTTP code
  , resPayload    :: Maybe a                   -- ^ Expected JSON payload
  , resHeaders    :: Map HeaderName ByteString -- ^ Returned HTTP headers
  , resRequest    :: Request                   -- ^ 'Network.HTTP.Simple' 'Request' data
  , resError      :: Maybe Auth0Error          -- ^ Auth0 error upon failure
  } deriving (Show)
