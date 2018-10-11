module Auth0.Authentication.WS where

--------------------------------------------------------------------------------
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------

-- WS - Federation
-- GET /wsfed/YOUR_CLIENT_ID

type WSApi
  =  Capture "user_id" Text
  :> QueryParam "wtrealm" Text
  :> QueryParam "whr" Text
  :> QueryParam "wctx" Text
  :> QueryParam "wreply" Text
  :> Get '[PlainText] Text

wsApi ::
     Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM Text

-- GET /wsfed/YOUR_CLIENT_ID/FederationMetadata/2007-06/FederationMetadata.xml

type WSMetaDataApi
  =  "FederationMetadata"
  :> "2007-06"
  :> "FederationMetadata.xml"
  :> Capture "client_id" Text
  :> Get '[PlainText] Text

wsMetaDataApi ::
     Text
  -> ClientM Text

type WSAllApi
  = "wsfed"
  :> (    WSApi
     :<|> WSMetaDataApi
     )

wsAllApi :: Proxy WSAllApi
wsAllApi = Proxy

wsApi
  :<|> wsMetaDataApi
  = client wsAllApi
