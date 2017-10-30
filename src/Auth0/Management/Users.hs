{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Auth0.Management.Users where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Map
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- GET /api/v2/users

-- Request

data User
  = User
  { perPage       :: Maybe Int
  , page          :: Maybe Int
  , includeTotals :: Maybe Bool
  , sort          :: Maybe Text
  , connection    :: Maybe Text
  , fields        :: Maybe Text
  , includeFields :: Maybe Text
  , q             :: Maybe Text
  , searchEngine  :: Maybe Text
  }

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
  { email         :: Maybe Text
  , emailVerified :: Maybe Bool
  , name          :: Maybe Text
  , username      :: Maybe Text
  , givenName     :: Maybe Text
  , phoneNumber   :: Maybe Text
  , phoneVerified :: Maybe Bool
  , familyName    :: Maybe Text
  } deriving (Generic)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''ProfileData

data Identity
  = Identity
  { connection  :: Text
  , userId      :: Text
  , provider    :: Text
  , isSocial    :: Bool
  , accessToken :: Maybe Text
  , profileData :: Maybe ProfileData
  } deriving (Generic)

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

data UserResponse
  = UserResponse
  { email         :: Maybe Text
  , emailVerified :: Maybe Bool
  , username      :: Maybe Text
  , phoneNumber   :: Maybe Text
  , phoneVerified :: Maybe Bool
  , userId        :: Maybe Text
  , createdAt     :: Maybe Text
  , updatedAt     :: Maybe Text
  , identities    :: Maybe [Identity]
  , appMetadata   :: Maybe (Map Text Text)
  , userMetadata  :: Maybe (Map Text Text)
  , picture       :: Maybe Text
  , name          :: Maybe Text
  , nickname      :: Maybe Text
  , multifactor   :: Maybe [Text]
  , lastIp        :: Maybe Text
  , lastLogin     :: Maybe Text
  , loginsCount   :: Maybe Int
  , blocked       :: Maybe Bool
  , givenName     :: Maybe Text
  , familyName    :: Maybe Text
  } deriving (Generic)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''UserResponse

runGetUsers
  :: (MonadIO m, MonadThrow m)
  => Host -> User -> m (Int, Maybe [UserResponse])
runGetUsers h o =
  let api = API Get "/api/v2/users"
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- POST /api/v2/users

-- Request

data UserCreate
  = UserCreate
  { userId        :: Maybe Text
  , connection    :: Text
  , email         :: Maybe Text
  , username      :: Maybe Text
  , password      :: Maybe Text
  , phoneNumber   :: Maybe Text
  , userMetadata  :: Maybe (Map Text Text)
  , emailVerified :: Maybe Bool
  , verifyEmail   :: Maybe Bool
  , phoneVerified :: Maybe Bool
  , appMetadata   :: Maybe (Map Text Text)
  } deriving (Generic)

instance ToJSON UserCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateUser
  :: (MonadIO m, MonadThrow m)
  => Host -> UserCreate -> m (Int, Maybe UserResponse)
runCreateUser h o =
  let api = API Post "/api/v2/users"
  in execRequest h api () o Nothing

---------------------------------------------------------------------------------
-- GET /api/v2/users/{id}

data UserGet
  = UserGet
  { id            :: Maybe Text
  , fields        :: Maybe Text
  , includeFields :: Maybe Text
  }

instance ToRequest UserGet where
  toRequest (UserGet a b c) =
    [ toField "id" a
    , toField "fields" b
    , toField "include_fields" c
    ]

runGetUser
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> UserGet -> m (Int, Maybe UserResponse)
runGetUser h i o =
  let api = API Get ("/api/v2/users/" <> encodeUtf8 i)
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/users/{id}

runDeleteUser
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe ())
runDeleteUser h i =
  let api = API Delete ("/api/v2/users/" <> encodeUtf8 i)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- PATCH /api/v2/users/{id}

runUpdateUser
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> UserCreate -> m (Int, Maybe UserResponse)
runUpdateUser h i o =
  let api = API Update ("/api/v2/users/" <> encodeUtf8 i)
  in execRequest h api () o Nothing

---------------------------------------------------------------------------------
-- GET /api/v2/users/{id}/enrollments

data UserEnrollment
  = UserEnrollment
  { id          :: Maybe Text
  , status      :: Maybe Text
  , etype       :: Maybe Text
  , name        :: Maybe Text
  , identifier  :: Maybe Text
  , phoneNumber :: Maybe Text
  , authMethod  :: Maybe Text
  , enrolledAt  :: Maybe Text
  , lastAuth    :: Maybe Text
  } deriving (Generic)

instance FromJSON UserEnrollment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = f }
    where
      f "type" = "etype"
      f v      = camelTo2 '_' v

runGetUserEnrollments
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe [UserEnrollment])
runGetUserEnrollments h i =
  let api = API Get ("/api/v2/users/" <> encodeUtf8 i <> "/enrollments")
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- GET /api/v2/users/{id}/logs

-- Request

data UserLogGet
  = UserLogGet
  { userId        :: Text
  , page          :: Maybe Int
  , perPage       :: Maybe Int
  , sort          :: Maybe Text
  , includeTotals :: Maybe Bool
  }

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
  { date         :: Maybe Text
  , ltype        :: Maybe Text
  , clientId     :: Maybe Text
  , clientName   :: Maybe Text
  , ip           :: Maybe Text
  , locationInfo :: Maybe (Map Text Text)
  , details      :: Maybe (Map Text Text)
  , userId       :: Maybe Text
  } deriving (Generic)

instance FromJSON UserLog where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetUserLogs
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> UserLogGet -> m (Int, Maybe [UserLog])
runGetUserLogs h i o =
  let api = API Get ("/api/v2/users/" <> encodeUtf8 i <> "/logs")
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/users/{id}/multifactor/{provider}

data MultifactorProvider = Duo | GoogleAuthenticator

instance Show MultifactorProvider where
  show Duo                 = "duo"
  show GoogleAuthenticator = "google-authenticator"

runDeleteUserProvider
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> MultifactorProvider -> m (Int, Maybe ())
runDeleteUserProvider h i j =
  let api = API Delete ("/api/v2/users/" <> encodeUtf8 i <>
                        "/multifactor/" <> (encodeUtf8 . pack . show) j)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/users/{id}/identities/{provider}/{user_id}

runDeleteUserIdentity
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> Text -> Text -> m (Int, Maybe Identity)
runDeleteUserIdentity h i j k =
  let api = API Delete ("/api/v2/users/" <> encodeUtf8 i <>
                        "/identities/" <> encodeUtf8 j <>
                        "/" <> encodeUtf8 k)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- POST /api/v2/users/{id}/recovery-code-regeneration

data GuardianRecoveryCode
  = GuardianRecoveryCode
  { recoveryCode :: Maybe Text
  } deriving (Generic)

instance FromJSON GuardianRecoveryCode where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runUserRecoveryCodeRegeneration
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe GuardianRecoveryCode)
runUserRecoveryCodeRegeneration h i =
  let api = API Post ("/api/v2/users/" <> encodeUtf8 i)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- POST /api/v2/users/{id}/identities

data LinkAccount
  = LinkAccount
  { provider     :: Maybe Text
  , connectionId :: Maybe Text
  , userId       :: Maybe Text
  , linkWith     :: Maybe Text
  } deriving (Generic)

instance ToJSON LinkAccount where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runUserLinkAccount
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> Text -> LinkAccount -> m (Int, Maybe [Identity])
runUserLinkAccount h i j o =
  let api = API Post ("/api/v2/users/" <> encodeUtf8 i <> "/identities")
      hdr = [("Authorization", "Bearer: " <> encodeUtf8 j)]
  in execRequest h api () o (Just hdr)
