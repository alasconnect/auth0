module Auth0.Management.UsersByEmail where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------
import Auth0.Types
import Auth0.Management.Users (UserResponse)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/users-by-email

type UsersByEmailGetApi appMd userMd
  =  Header' '[Required] "Authorization" AccessToken
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Text
  :> QueryParam' '[Required]  "email" Text
  :> Get '[JSON] [UserResponse appMd userMd]

usersByEmailGet :: (FromJSON appMd, ToJSON appMd, FromJSON userMd, ToJSON userMd)
  => AccessToken
  -> Maybe Text
  -> Maybe Text
  -> Text
  -> ClientM [UserResponse appMd userMd]

--------------------------------------------------------------------------------

type UsersByEmailApi appMd userMd
  = "api"
  :> "v2"
  :> "users-by-email"
  :> UsersByEmailGetApi appMd userMd

usersByEmailApi :: Proxy (UsersByEmailApi appMd userMd)
usersByEmailApi = Proxy

usersByEmailGet = client usersByEmailApi
