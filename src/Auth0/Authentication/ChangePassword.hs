module Auth0.Authentication.ChangePassword where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------

data ChangePassword
  = ChangePassword
  { clientId   :: Text
  , email      :: Text
  , password   :: Maybe Text
  , connection :: Text
  } deriving (Generic, Show)

instance ToJSON ChangePassword where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

type ChangePasswordApi
  = "dbconnections"
  :> "change_password"
  :> ReqBody '[JSON] ChangePassword
  :> Post '[JSON] NoContent

changePasswordApi :: Proxy ChangePasswordApi
changePasswordApi = Proxy

changePassword ::
     ChangePassword
  -> ClientM NoContent

changePassword = client changePasswordApi
