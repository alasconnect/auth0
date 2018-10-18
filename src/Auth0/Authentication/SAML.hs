module Auth0.Authentication.SAML where

--------------------------------------------------------------------------------
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------

-- GET /samlp/YOUR_CLIENT_ID

type SAMLAcceptRequestIdApi
  =  QueryParam' '[Required] "client_id" Text
  :> Get '[PlainText] Text

samlAcceptRequestId ::
     Text
  -> ClientM Text

-- GET /samlp/metadata/YOUR_CLIENT_ID

type SAMLAcceptRequestMetaDataApi
  = "metadata"
  :> QueryParam' '[Required] "client_id" Text
  :> Get '[PlainText] Text

type SAMLAcceptRequestApi
  = "samlp"
  :> (    SAMLAcceptRequestIdApi
     :<|> SAMLAcceptRequestMetaDataApi
     )

samlAcceptRequestApi :: Proxy SAMLAcceptRequestApi
samlAcceptRequestApi = Proxy

samlAcceptRequestMetaData ::
     Text
  -> ClientM Text

samlAcceptRequestId
  :<|> samlAcceptRequestMetaData
  = client samlAcceptRequestApi

-- IdP - Initiated SSO Flow
-- POST /login/callback

type IdPApi
  =  "login"
  :> "callback"
  :> QueryParam' '[Required] "connection" Text
  :> QueryParam' '[Required] "saml_response" Text
  :> Post '[PlainText] Text

idPApi :: Proxy IdPApi
idPApi = Proxy

idP ::
     Text
  -> Text
  -> ClientM Text

idP = client idPApi
