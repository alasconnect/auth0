module Auth0.Authentication.Passwordless where

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

-- POST /passwordless/start

data ConnectionType
  = TEmail
  | TSms
  deriving (Show)

instance ToJSON ConnectionType where
  toJSON TEmail = "email"
  toJSON TSms   = "sms"

data SendType
  = TLink
  | TCode
  deriving (Show)

instance ToJSON SendType where
  toJSON TLink = "link"
  toJSON TCode = "code"

data GetCodeOrLink
  = GetCodeOrLink
  { clientId    :: Text
  , connection  :: ConnectionType
  , email       :: Maybe Text
  , phoneNumber :: Maybe Text
  , send        :: Maybe SendType
  , authParams  :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON GetCodeOrLink where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type GetCodeOrLinkApi
  =  "passwordless"
  :> "start"
  :> ReqBody '[JSON] GetCodeOrLink
  :> Post '[JSON] NoContent

getCodeOrLinkApi :: Proxy GetCodeOrLinkApi
getCodeOrLinkApi = Proxy

getCodeOrLink ::
     GetCodeOrLink
  -> ClientM NoContent

getCodeOrLink = client getCodeOrLinkApi

-- POST /oauth/ro

data AuthenticateUser
  = AuthenticateUser
  { clientId   :: Text
  , connection :: ConnectionType
  , grantType  :: GrantType
  , username   :: Text
  , password   :: Text
  , scope      :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON AuthenticateUser where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type AuthenticateUserApi
  =  "oath"
  :> "ro"
  :> ReqBody '[JSON] AuthenticateUser
  :> Post '[JSON] NoContent

authenticateUserApi :: Proxy AuthenticateUserApi
authenticateUserApi = Proxy

authenticateUser ::
     AuthenticateUser
  -> ClientM NoContent

authenticateUser = client authenticateUserApi
