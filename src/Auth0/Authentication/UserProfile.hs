module Auth0.Authentication.UserProfile where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------

-- GET /userinfo

data UserProfileResponse
  = UserProfileResponse
  { emailVerified :: Bool
  , email         :: Text
  , clientID      :: Text
  , updatedAt     :: Text
  , name          :: Text
  , picture       :: Text
  , userId        :: Text
  , nickname      :: Text
  , createdAt     :: Text
  , sub           :: Text
  } deriving (Generic, Show)

instance FromJSON UserProfileResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type UserProfileApi
  = "userinfo"
  :> Get '[JSON] UserProfileResponse

userProfileApi :: Proxy UserProfileApi
userProfileApi = Proxy

userProfile :: ClientM UserProfileResponse

userProfile = client userProfileApi
