module Auth0.Authentication.Login where

--------------------------------------------------------------------------------
--import Data.Map
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------
import Auth0.Types
--------------------------------------------------------------------------------

type LoginApi
  = "authorize"
  :> QueryParam' '[Required] "response_type" ResponseType
  :> QueryParam' '[Required] "client_id" Text
  :> QueryParam "connection" Text
  :> QueryParam' '[Required]  "redirect_uri" Text
  :> QueryParam "state" Text
  -- :> QueryParams "additional" (Map Text Text)
  :> Get '[JSON] NoContent

loginApi :: Proxy LoginApi
loginApi = Proxy

login ::
     ResponseType
  -> Text
  -> Maybe Text
  -> Text
  -> Maybe Text
  -- -> [Map Text Text]
  -> ClientM NoContent

login = client loginApi
