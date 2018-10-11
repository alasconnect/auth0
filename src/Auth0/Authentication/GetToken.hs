module Auth0.Authentication.GetToken where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------
import Auth0.Types
--------------------------------------------------------------------------------

data GetTokenResponse
  = GetTokenResponse
  { accessToken  :: AccessToken
  , refreshToken :: Maybe Text
  , idToken      :: Maybe Text
  , tokenType    :: Text
  , expiresIn    :: Int
  , recoveryCode :: Maybe Text
  , scope        :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON GetTokenResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- POST /oauth/token

-- Authorize Code

data GetToken
  = GetToken
  { grantType    :: GrantType
  , clientId     :: ClientId
  , clientSecret :: ClientSecret
  , code         :: Text
  , redirectUri  :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON GetToken where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GetTokenApi
  =  ReqBody '[JSON] GetToken
  :> Post '[JSON] GetTokenResponse

getToken ::
     GetToken
  -> ClientM GetTokenResponse

--------------------------------------------------------------------------------
-- POST /oauth/token

-- Authorize Code (PKCE)

data GetTokenPKCE
  = GetTokenPKCE
  { grantType    :: GrantType
  , clientId     :: ClientId
  , code         :: Text
  , codeVerifier :: Text
  , redirectUri  :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON GetTokenPKCE where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GetTokenPKCEApi
  =  ReqBody '[JSON] GetTokenPKCE
  :> Post '[JSON] GetTokenResponse

getTokenPKCE ::
     GetTokenPKCE
  -> ClientM GetTokenResponse

--------------------------------------------------------------------------------
-- POST /oauth/token

-- Client Credentials

data GetTokenClientCreds
  = GetTokenClientCreds
  { grantType    :: GrantType
  , clientId     :: ClientId
  , clientSecret :: ClientSecret
  , audience     :: Text
  } deriving (Generic, Show)

instance ToJSON GetTokenClientCreds where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GetTokenClientCredsApi
  =  ReqBody '[JSON] GetTokenClientCreds
  :> Post '[JSON] GetTokenResponse

getTokenClientCreds ::
     GetTokenClientCreds
  -> ClientM GetTokenResponse

--------------------------------------------------------------------------------
-- POST /oauth/token

-- Resource Owner Password

data GetTokenResourceOwner
  = GetTokenResourceOwner
  { grantType    :: GrantType
  , clientId     :: ClientId
  , clientSecret :: Maybe ClientSecret
  , audience     :: Maybe Text
  , username     :: Text
  , password     :: Text
  , scope        :: Maybe Text
  , realm        :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON GetTokenResourceOwner where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GetTokenResourceOwnerApi
  =  ReqBody '[JSON] GetTokenResourceOwner
  :> Post '[JSON] GetTokenResponse

getTokenResourceOwner ::
     GetTokenResourceOwner
  -> ClientM GetTokenResponse

--------------------------------------------------------------------------------

type GetTokenAllApi
  =  "oauth"
  :> "token" :>
  (
       GetTokenApi
  :<|> GetTokenPKCEApi
  :<|> GetTokenClientCredsApi
  :<|> GetTokenResourceOwnerApi
  )

getTokenApi :: Proxy GetTokenAllApi
getTokenApi = Proxy

getToken
  :<|> getTokenPKCE
  :<|> getTokenClientCreds
  :<|> getTokenResourceOwner
  = client getTokenApi

--------------------------------------------------------------------------------
-- POST /mfa/challenge

-- Resource Owner Password and MFA

data GetTokenResourceOwnerMFA
  = GetTokenResourceOwnerMFA
  { mfaToken      :: Text
  , clientId      :: ClientId
  , clientSecret  :: Maybe ClientSecret
  , challengeType :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON GetTokenResourceOwnerMFA where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GetTokenResourceOwnerMFAApi
  = "/mfa/challenge"
  :> ReqBody '[JSON] GetTokenResourceOwnerMFA
  :> Post '[JSON] GetTokenResourceOwnerMFAResponse

getTokenResourceOwnerMFA ::
     GetTokenResourceOwnerMFA
  -> ClientM GetTokenResourceOwnerMFAResponse

-- Response

data GetTokenResourceOwnerMFAResponse
  = GetTokenResourceOwnerMFAResponse
  { challengeType :: Text
  , bindingMethod :: Maybe Text
  , oobCode       :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON GetTokenResourceOwnerMFAResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

getTokenResourceOwnerMFAApi :: Proxy GetTokenResourceOwnerMFAApi
getTokenResourceOwnerMFAApi = Proxy

getTokenResourceOwnerMFA = client getTokenResourceOwnerMFAApi

--------------------------------------------------------------------------------
-- TODO: The following
--------------------------------------------------------------------------------
-- POST /oauth/token

-- Verify MFA using OTP

data GetTokenVerifyMFAOTP
  = GetTokenVerifyMFAOTP
  { grantType    :: GrantType
  , clientId     :: ClientId
  , clientSecret :: Maybe ClientSecret
  , mfaToken     :: Text
  , otp          :: Text
  } deriving (Generic, Show)

instance ToJSON GetTokenVerifyMFAOTP where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- POST /oauth/token

-- Verify MFA using OOB challenge

data GetTokenVerifyMFAOOB
  = GetTokenVerifyMFAOOB
  { grantType    :: GrantType
  , clientId     :: ClientId
  , clientSecret :: Maybe ClientSecret
  , mfaToken     :: Text
  , oobCode      :: Text
  , bindingCode  :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON GetTokenVerifyMFAOOB where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

-- Verify MFA using a recovery code

data GetTokenVerifyRecoveryCode
  = GetTokenVerifyRecoveryCode
  { grantType    :: GrantType
  , clientId     :: ClientId
  , clientSecret :: Maybe ClientSecret
  , mfaToken     :: Text
  , recoveryCode :: Text
  } deriving (Generic, Show)

instance ToJSON GetTokenVerifyRecoveryCode where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

-- Refresh Token

data GetTokenRefresh
  = GetTokenRefresh
  { grantType    :: GrantType
  , clientId     :: ClientId
  , clientSecret :: Maybe ClientSecret
  , refreshToken :: Text
  } deriving (Generic, Show)

instance ToJSON GetTokenRefresh where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }
