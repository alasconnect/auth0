module Auth0.Authentication.Impersonation where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Map
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------

data Protocol
  = OAuth2
  | SAMLP
  | WSFed
  | WSFedRMS
  deriving (Generic)

instance Show Protocol where
  show OAuth2   = "oauth2"
  show SAMLP    = "samlp"
  show WSFed    = "wsfed"
  show WSFedRMS = "wsfed-rms"

instance ToJSON Protocol where
  toJSON OAuth2   = "oauth2"
  toJSON SAMLP    = "samlp"
  toJSON WSFed    = "wsfed"
  toJSON WSFedRMS = "wsfed-rms"

-- POST /users/{user_id}/impersonate

data Impersonate
  = Impersonate
  { protocol             :: Protocol
  , impersonatorId       :: Text
  , clientId             :: Text
  , additionalParameters :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON Impersonate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type ImpersonateApi
  = "users"
  :> Capture "user_id" Text
  :> "impersonate"
  :> ReqBody '[JSON] Impersonate
  :> Post '[PlainText] Text

impersonateApi :: Proxy ImpersonateApi
impersonateApi = Proxy

impersonate ::
     Text
  -> Impersonate
  -> ClientM Text

impersonate = client impersonateApi
