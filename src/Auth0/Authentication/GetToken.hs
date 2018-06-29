{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Authentication.GetToken where

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- Request Headers

data GetTokenResourceOwnerHeader
  = GetTokenResourceOwnerHeader
  { auth0ForwardFor :: Text
  } deriving (Show)

-- Response

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
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetToken
  :: (MonadIO m, MonadThrow m, ToJSON a, Show a)
  => Auth -> a -> m (Auth0Response GetTokenResponse)
runGetToken (Auth tenant) o =
  let api = API Post "/oauth/token"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) Nothing

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- Response

data GetTokenResourceOwnerMFAResponse
  = GetTokenResourceOwnerMFAResponse
  { challengeType :: Text
  , bindingMethod :: Maybe Text
  , oobCode       :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON GetTokenResourceOwnerMFAResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetTokenMFA
  :: (MonadIO m, MonadThrow m)
  => Auth -> GetTokenResourceOwnerMFA
  -> m (Auth0Response GetTokenResourceOwnerMFAResponse)
runGetTokenMFA (Auth tenant) o =
  let api = API Post "/mfa/challenge"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) Nothing

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
