module Auth0.Authentication.Logout where

--------------------------------------------------------------------------------
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------

-- GET /v2/logout

type LogoutApi
  =  "v2"
  :> "logout"
  :> QueryParam "return_to" Text
  :> QueryParam "client_id" Text
  :> QueryParam "federated" Text
  :> Get '[JSON] NoContent

logoutApi :: Proxy LogoutApi
logoutApi = Proxy

logout ::
     Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM NoContent

logout = client logoutApi
