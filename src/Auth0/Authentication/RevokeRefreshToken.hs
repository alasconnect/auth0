module Auth0.Authentication.RevokeRefreshToken where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------
import Auth0.Types
--------------------------------------------------------------------------------

-- POST /oauth/revoke

data RevokeRefreshToken
  = RevokeRefreshToken
  { clientId     :: ClientId
  , clientSecret :: Maybe ClientSecret
  , token        :: Text
  } deriving (Generic, Show)

instance ToJSON RevokeRefreshToken where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type RevokeRefreshTokenApi
  =  "oauth"
  :> "revoke"
  :> ReqBody '[JSON] RevokeRefreshToken
  :> Post '[JSON] NoContent

revokeRefreshTokenApi :: Proxy RevokeRefreshTokenApi
revokeRefreshTokenApi = Proxy

revokeRefreshToken ::
     RevokeRefreshToken
  -> ClientM NoContent

revokeRefreshToken = client revokeRefreshTokenApi
