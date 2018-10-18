module Auth0.Authentication.Signup where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Map
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------
import Auth0.Types
--------------------------------------------------------------------------------

-- POST /dbconnections/signup

data Signup
  = Signup
  { clientId     :: ClientId
  , email        :: Text
  , password     :: Text
  , connection   :: Text
  , userMetadata :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON Signup where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

data SignupResponse
  = SignupResponse
  { _id           :: Text
  , emailVerified :: Bool
  , email         :: Text
  } deriving (Generic, Show)

instance FromJSON SignupResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type SignupApi
  =  "dbconnections"
  :> "signup"
  :> ReqBody '[JSON] Signup
  :> Post '[JSON] SignupResponse

signupApi :: Proxy SignupApi
signupApi = Proxy

signup ::
     Signup
  -> ClientM SignupResponse

signup = client signupApi
