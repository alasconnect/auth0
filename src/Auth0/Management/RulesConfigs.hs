module Auth0.Management.RulesConfigs where

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

data RuleConfigResponse
  = RuleConfigResponse
  { key :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON RuleConfigResponse

--------------------------------------------------------------------------------
-- GET /api/v2/rules-configs

type RulesConfigsGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Get '[JSON] [RuleConfigResponse]

rulesConfigsGet ::
     AccessToken
  -> ClientM [RuleConfigResponse]

--------------------------------------------------------------------------------
-- DELETE /api/v2/rules-configs/{key}

type RulesConfigsDeleteApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Delete '[JSON] NoContent

rulesConfigsDelete ::
     AccessToken
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- PUT /api/v2/rules-configs/{key}

data RuleConfigSet
  = RuleConfigSet
  { value :: Text
  } deriving (Generic, Show)

instance ToJSON RuleConfigSet

data RuleConfigSetResponse
  = RuleConfigSetResponse
  { key   :: Text
  , value :: Text
  } deriving (Generic, Show)

instance FromJSON RuleConfigSetResponse

type RulesConfigsUpdateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> ReqBody '[JSON] RuleConfigSet
  :> Put '[JSON] RuleConfigSetResponse

rulesConfigsUpdate ::
     AccessToken
  -> Text
  -> RuleConfigSet
  -> ClientM RuleConfigSetResponse

--------------------------------------------------------------------------------

type RulesConfigsApi
  =  "api"
  :> "v2"
  :> "rules-configs"
  :> (    RulesConfigsGetApi
     :<|> RulesConfigsDeleteApi
     :<|> RulesConfigsUpdateApi
     )

rulesConfigsApi :: Proxy RulesConfigsApi
rulesConfigsApi = Proxy

rulesConfigsGet
  :<|> rulesConfigsDelete
  :<|> rulesConfigsUpdate
  = client rulesConfigsApi
