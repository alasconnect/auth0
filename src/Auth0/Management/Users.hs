{-# LANGUAGE NoMonomorphismRestriction #-}

module Auth0.Management.Users where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Proxy
import Data.Map hiding (drop)
import Data.Text hiding (drop)
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/users

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

instance ToJSON ProfileData where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance FromJSON ProfileData where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

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
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = f . drop 1 }
    where
      f "IsSocial" = "isSocial"
      f v          = camelTo2 '_' v

instance ToJSON Identity where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = f . drop 1 }
    where
      f "IsSocial" = "isSocial"
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

instance (ToJSON appMd, ToJSON userMd) => ToJSON (UserResponse appMd userMd) where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

instance (FromJSON appMd, FromJSON userMd) => FromJSON (UserResponse appMd userMd) where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type UsersGetApi appMd userMd
  =  Header' '[Required] "Authorization" AccessToken
  :> QueryParam "per_page" Int
  :> QueryParam "page" Int
  :> QueryParam "include_totals" Bool
  :> QueryParam "sort" Text
  :> QueryParam "connection" Text
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Text
  :> QueryParam "q" Text
  :> QueryParam "search_engine" Text
  :> Get '[JSON] [UserResponse appMd userMd]

usersGet :: (FromJSON appMd, ToJSON appMd, FromJSON userMd, ToJSON userMd)
  => AccessToken
  -> Maybe Int
  -> Maybe Int
  -> Maybe Bool
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM [UserResponse appMd userMd]

--------------------------------------------------------------------------------
-- POST /api/v2/users

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
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' . drop 2 }

type UserCreateApi appMd userMd
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] (UserCreate appMd userMd)
  :> Post '[JSON] [UserResponse appMd userMd]

userCreate :: (FromJSON appMd, ToJSON appMd, FromJSON userMd, ToJSON userMd)
  => AccessToken
  -> UserCreate appMd userMd
  -> ClientM [UserResponse appMd userMd]

--------------------------------------------------------------------------------
-- GET /api/v2/users/{id}

type UserGetApi appMd userMd
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Text
  :> Get '[JSON] (UserResponse appMd userMd)

userGet :: (FromJSON appMd, ToJSON appMd, FromJSON userMd, ToJSON userMd)   
  => AccessToken
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM (UserResponse appMd userMd)

--------------------------------------------------------------------------------
-- DELETE /api/v2/users/{id}

type UserDeleteApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Delete '[JSON] NoContent

userDelete ::
     AccessToken
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- PATCH /api/v2/users/{id}

type UserUpdateApi appMd userMd
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> ReqBody '[JSON] (UserCreate appMd userMd)
  :> Patch '[JSON] (UserResponse appMd userMd)

userUpdate :: (FromJSON appMd, ToJSON appMd, FromJSON userMd, ToJSON userMd)
  => AccessToken
  -> Text
  -> UserCreate appMd userMd
  -> ClientM (UserResponse appMd userMd)

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
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = f . drop 2 }
    where
      f "type" = "etype"
      f v      = camelTo2 '_' v

type UserEnrollmentGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Get '[JSON] [UserEnrollment]

userEnrollmentGet ::
     AccessToken
  -> Text
  -> ClientM [UserEnrollment]

--------------------------------------------------------------------------------
-- GET /api/v2/users/{id}/logs

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
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' . drop 2 }

type UserLogGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> QueryParam' '[Required] "user_id" Text
  :> QueryParam "page" Int
  :> QueryParam "per_page" Int
  :> QueryParam "sort" Text
  :> QueryParam "include_totals" Bool
  :> Get '[JSON] [UserLog]

userLogGet ::
     AccessToken
  -> Text
  -> Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe Bool
  -> ClientM [UserLog]

--------------------------------------------------------------------------------
-- DELETE /api/v2/users/{id}/multifactor/{provider}

data MultifactorProvider = Duo | GoogleAuthenticator

instance ToHttpApiData MultifactorProvider where
  toUrlPiece Duo = "duo"
  toUrlPiece GoogleAuthenticator = "google-authenticator"

type UserMultifactorProviderDeleteApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> "multifactor"
  :> Capture "provider" MultifactorProvider
  :> Delete '[JSON] NoContent

userMultifactorProviderDelete ::
     AccessToken
  -> Text
  -> MultifactorProvider
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- DELETE /api/v2/users/{id}/identities/{provider}/{user_id}

type UserMultifactorProviderDeleteUserApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> "identities"
  :> Capture "provider" MultifactorProvider
  :> Capture "user_id" Text
  :> Delete '[JSON] Identity

userMultifactorProviderDeleteUser ::
     AccessToken
  -> Text
  -> MultifactorProvider
  -> Text
  -> ClientM Identity

--------------------------------------------------------------------------------
-- POST /api/v2/users/{id}/recovery-code-regeneration

data GuardianRecoveryCode
  = GuardianRecoveryCode
  { recoveryCode :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON GuardianRecoveryCode where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type UserRecoveryCodeCreateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> "recovery-code-regeneration"
  :> Post '[JSON] GuardianRecoveryCode

userRecoveryCodeCreate ::
     AccessToken
  -> Text
  -> ClientM GuardianRecoveryCode

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
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' . drop 2 }

type UserLinkAccountApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] LinkAccount
  :> Post '[JSON] [Identity]

userLinkAccount ::
     AccessToken
  -> LinkAccount
  -> ClientM [Identity]

--------------------------------------------------------------------------------

type UsersPolyApi appMd userMd
  =  "api"
  :> "v2"
  :> "users"
  :> (    UsersGetApi appMd userMd
     :<|> UserCreateApi appMd userMd
     :<|> UserGetApi appMd userMd
     :<|> UserUpdateApi appMd userMd
     )

type UsersApi
  =  "api"
  :> "v2"
  :> "users"
  :> (    UserDeleteApi
     :<|> UserEnrollmentGetApi
     :<|> UserLogGetApi
     :<|> UserMultifactorProviderDeleteApi
     :<|> UserMultifactorProviderDeleteUserApi
     :<|> UserRecoveryCodeCreateApi
     :<|> UserLinkAccountApi
     )

usersPolyApi :: Proxy (UsersPolyApi appMd userMd)
usersPolyApi = Proxy

usersApi :: Proxy UsersApi
usersApi = Proxy

usersGet
  :<|> userCreate
  :<|> userGet
  :<|> userUpdate
  = client usersPolyApi

userDelete
  :<|> userEnrollmentGet
  :<|> userLogGet
  :<|> userMultifactorProviderDelete
  :<|> userMultifactorProviderDeleteUser
  :<|> userRecoveryCodeCreate
  :<|> userLinkAccount
  = client usersApi
