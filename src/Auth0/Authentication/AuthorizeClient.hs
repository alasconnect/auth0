module Auth0.Authentication.AuthorizeClient where

--------------------------------------------------------------------------------
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------
import Auth0.Types
--------------------------------------------------------------------------------

type CodeGrantApi
  =  QueryParam "audience" Text
  :> QueryParam "scope" Text
  :> QueryParam' '[Required] "response_type" ResponseType
  :> QueryParam' '[Required] "client_id" Text
  :> QueryParam "redirect_uri" Text
  :> QueryParam "state" Text
  :> QueryParam "prompt" Text
  :> Get '[JSON] NoContent

codeGrant ::
     Maybe Text
  -> Maybe Text
  -> ResponseType
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM NoContent

type CodeGrantPKCEApi
  =  QueryParam "audience" Text
  :> QueryParam "scope" Text
  :> QueryParam' '[Required] "response_type" ResponseType
  :> QueryParam' '[Required] "client_id" Text
  :> QueryParam "state" Text
  :> QueryParam "redirect_uri" Text
  :> QueryParam' '[Required] "code_challenge_method" Text
  :> QueryParam' '[Required] "code_challenge" Text
  :> QueryParam "prompt" Text
  :> Get '[JSON] NoContent

codeGrantPKCE ::
     Maybe Text
  -> Maybe Text
  -> ResponseType
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Text
  -> Text
  -> Maybe Text
  -> ClientM NoContent

type CodeGrantImplicitApi
  =  QueryParam "audience" Text
  :> QueryParam "scope" Text
  :> QueryParam' '[Required] "response_type" ResponseType
  :> QueryParam' '[Required] "client_id" Text
  :> QueryParam "state" Text
  :> QueryParam "redirect_uri" Text
  :> QueryParam "nonce" Text
  :> QueryParam "connection" Text
  :> QueryParam "prompt" Text
  :> Get '[JSON] NoContent

codeGrantImplicit ::
     Maybe Text
  -> Maybe Text
  -> ResponseType
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM NoContent

type CodeGrantAllApi
  = "authorize"
  :> (    CodeGrantApi
     :<|> CodeGrantPKCEApi
     :<|> CodeGrantImplicitApi
     )

codeGrantApi :: Proxy CodeGrantAllApi
codeGrantApi = Proxy

codeGrant
  :<|> codeGrantPKCE
  :<|> codeGrantImplicit
  = client codeGrantApi
