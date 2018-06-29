{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Auth0.Management.Users where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Map
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/users

-- Request

data User
  = User
  { uPerPage       :: Maybe Int
  , uPage          :: Maybe Int
  , uIncludeTotals :: Maybe Bool
  , uSort          :: Maybe Text
  , uConnection    :: Maybe Text
  , uFields        :: Maybe Text
  , uIncludeFields :: Maybe Text
  , uQ             :: Maybe Text
  , uSearchEngine  :: Maybe Text
  } deriving (Show)

instance ToRequest User where
  toRequest (User a b c d e f g h i) =
    [ toField "per_page" a
    , toField "page" b
    , toField "include_totals" c
    , toField "sort" d
    , toField "connection" e
    , toField "fields" f
    , toField "include_fields" g
    , toField "q" h
    , toField "search_engine" i
    ]

-- Response

data ProfileData
  = ProfileData
  { pdEmail         :: Maybe Text
  , pdEmailVerified :: Maybe Bool
  , pdName          :: Maybe Text
  , pdUsername      :: Maybe Text
  , pdGivenName     :: Maybe Text
  , pdPhoneNumber   :: Maybe Text
  , pdPhoneVerified :: Maybe Bool
  , pdFamilyName    :: Maybe Text
  } deriving (Generic, Show)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''ProfileData

data Identity
  = Identity
  { iConnection  :: Text
  , iUserId      :: Text
  , iProvider    :: Text
  , iIsSocial    :: Bool
  , iAccessToken :: Maybe Text
  , iProfileData :: Maybe ProfileData
  } deriving (Generic, Show)

instance FromJSON Identity where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = f }
    where
      f "isSocial" = "isSocial"
      f v          = camelTo2 '_' v

instance ToJSON Identity where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = f }
    where
      f "isSocial" = "isSocial"
      f v          = camelTo2 '_' v

data UserResponse appMd userMd
  = UserResponse
  { urEmail         :: Maybe Text
  , urEmailVerified :: Maybe Bool
  , urUsername      :: Maybe Text
  , urPhoneNumber   :: Maybe Text
  , urPhoneVerified :: Maybe Bool
  , urUserId        :: Maybe Text
  , urCreatedAt     :: Maybe Text
  , urUpdatedAt     :: Maybe Text
  , urIdentities    :: Maybe [Identity]
  , urAppMetadata   :: Maybe appMd
  , urUserMetadata  :: Maybe userMd
  , urPicture       :: Maybe Text
  , urName          :: Maybe Text
  , urNickname      :: Maybe Text
  , urMultifactor   :: Maybe [Text]
  , urLastIp        :: Maybe Text
  , urLastLogin     :: Maybe Text
  , urLoginsCount   :: Maybe Int
  , urBlocked       :: Maybe Bool
  , urGivenName     :: Maybe Text
  , urFamilyName    :: Maybe Text
  } deriving (Generic, Show)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''UserResponse

runGetUsers
  :: (MonadIO m, MonadThrow m, FromJSON appMd, FromJSON userMd)
  => TokenAuth -> Maybe User -> m (Auth0Response [UserResponse appMd userMd])
runGetUsers (TokenAuth tenant accessToken) o =
  let api = API Get "/api/v2/users"
  in execRequest tenant api o (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/users

-- Request

data UserCreate appMd userMd
  = UserCreate
  { ucUserId        :: Maybe Text
  , ucConnection    :: Text
  , ucEmail         :: Maybe Text
  , ucUsername      :: Maybe Text
  , ucPassword      :: Maybe Text
  , ucPhoneNumber   :: Maybe Text
  , ucUserMetadata  :: Maybe userMd
  , ucEmailVerified :: Maybe Bool
  , ucVerifyEmail   :: Maybe Bool
  , ucPhoneVerified :: Maybe Bool
  , ucAppMetadata   :: Maybe appMd
  } deriving (Generic, Show)

instance (ToJSON appMd, ToJSON userMd) => ToJSON (UserCreate appMd userMd) where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateUser
  :: (MonadIO m, MonadThrow m, FromJSON appMd, FromJSON userMd, ToJSON appMd, ToJSON userMd, Show appMd, Show userMd)
  => TokenAuth -> UserCreate appMd userMd -> m (Auth0Response (UserResponse appMd userMd))
runCreateUser (TokenAuth tenant accessToken) o =
  let api = API Post "/api/v2/users"
      authHeader = Just [mkAuthHeader accessToken]
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) authHeader

--------------------------------------------------------------------------------
-- GET /api/v2/users/{id}

data UserGet
  = UserGet
  { fields        :: Maybe Text
  , includeFields :: Maybe Text
  } deriving (Show)

instance ToRequest UserGet where
  toRequest (UserGet a b) =
    [ toField "fields" a
    , toField "include_fields" b
    ]

runGetUser
  :: (MonadIO m, MonadThrow m, FromJSON appMd, FromJSON userMd)
  => TokenAuth -> Text -> Maybe UserGet -> m (Auth0Response (UserResponse appMd userMd))
runGetUser (TokenAuth tenant accessToken) i o =
  let api = API Get ("/api/v2/users/" <> encodeUtf8 i)
      authHeader = Just [mkAuthHeader accessToken]
  in execRequest tenant api o (Nothing :: Maybe ()) authHeader

--------------------------------------------------------------------------------
-- DELETE /api/v2/users/{id}

runDeleteUser
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response ())
runDeleteUser (TokenAuth tenant accessToken) i =
  let api = API Delete ("/api/v2/users/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- PATCH /api/v2/users/{id}

runUpdateUser
  :: (MonadIO m, MonadThrow m, FromJSON appMd, FromJSON userMd, ToJSON appMd, ToJSON userMd, Show appMd, Show userMd)
  => TokenAuth -> Text -> UserCreate appMd userMd -> m (Auth0Response (UserResponse appMd userMd))
runUpdateUser (TokenAuth tenant accessToken) i o =
  let api = API Update ("/api/v2/users/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- GET /api/v2/users/{id}/enrollments

data UserEnrollment
  = UserEnrollment
  { ueId          :: Maybe Text
  , ueStatus      :: Maybe Text
  , ueEtype       :: Maybe Text
  , ueName        :: Maybe Text
  , ueIdentifier  :: Maybe Text
  , uePhoneNumber :: Maybe Text
  , ueAuthMethod  :: Maybe Text
  , ueEnrolledAt  :: Maybe Text
  , ueLastAuth    :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON UserEnrollment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = f }
    where
      f "type" = "etype"
      f v      = camelTo2 '_' v

runGetUserEnrollments
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response [UserEnrollment])
runGetUserEnrollments (TokenAuth tenant accessToken) i =
  let api = API Get ("/api/v2/users/" <> encodeUtf8 i <> "/enrollments")
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- GET /api/v2/users/{id}/logs

-- Request

data UserLogGet
  = UserLogGet
  { ulgUserId        :: Text
  , ulgPage          :: Maybe Int
  , ulgPerPage       :: Maybe Int
  , ulgSort          :: Maybe Text
  , ulgIncludeTotals :: Maybe Bool
  } deriving (Show)

instance ToRequest UserLogGet where
  toRequest (UserLogGet a b c d e) =
    [ toField "user_id" a
    , toField "page" b
    , toField "per_page" c
    , toField "sort" d
    , toField "include_totals" e
    ]

-- Response

data UserLog
  = UserLog
  { ulDate         :: Maybe Text
  , ulLtype        :: Maybe Text
  , ulClientId     :: Maybe Text
  , ulClientName   :: Maybe Text
  , ulIp           :: Maybe Text
  , ulLocationInfo :: Maybe (Map Text Text)
  , ulDetails      :: Maybe (Map Text Text)
  , ulUserId       :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON UserLog where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetUserLogs
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> UserLogGet -> m (Auth0Response [UserLog])
runGetUserLogs (TokenAuth tenant accessToken) i o =
  let api = API Get ("/api/v2/users/" <> encodeUtf8 i <> "/logs")
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- DELETE /api/v2/users/{id}/multifactor/{provider}

data MultifactorProvider = Duo | GoogleAuthenticator

instance Show MultifactorProvider where
  show Duo                 = "duo"
  show GoogleAuthenticator = "google-authenticator"

runDeleteUserProvider
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> MultifactorProvider -> m (Auth0Response ())
runDeleteUserProvider (TokenAuth tenant accessToken) i j =
  let api = API Delete ("/api/v2/users/" <> encodeUtf8 i <>
                        "/multifactor/" <> (encodeUtf8 . pack . show) j)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- DELETE /api/v2/users/{id}/identities/{provider}/{user_id}

runDeleteUserIdentity
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> Text -> Text -> m (Auth0Response Identity)
runDeleteUserIdentity (TokenAuth tenant accessToken)  i j k =
  let api = API Delete ("/api/v2/users/" <> encodeUtf8 i <>
                        "/identities/" <> encodeUtf8 j <>
                        "/" <> encodeUtf8 k)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/users/{id}/recovery-code-regeneration

data GuardianRecoveryCode
  = GuardianRecoveryCode
  { recoveryCode :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON GuardianRecoveryCode where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runUserRecoveryCodeRegeneration
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response GuardianRecoveryCode)
runUserRecoveryCodeRegeneration (TokenAuth tenant accessToken) i =
  let api = API Post ("/api/v2/users/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/users/{id}/identities

data LinkAccount
  = LinkAccount
  { laProvider     :: Maybe Text
  , laConnectionId :: Maybe Text
  , laUserId       :: Maybe Text
  , laLinkWith     :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON LinkAccount where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runUserLinkAccount
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> LinkAccount -> m (Auth0Response [Identity])
runUserLinkAccount (TokenAuth tenant accessToken) i o =
  let api = API Post ("/api/v2/users/" <> encodeUtf8 i <> "/identities")
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])
