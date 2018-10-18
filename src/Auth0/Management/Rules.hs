module Auth0.Management.Rules where

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

data RuleResponse
  = RuleResponse
  { name    :: Maybe Text
  , id      :: Maybe Text
  , enabled :: Maybe Bool
  , script  :: Maybe Text
  , number  :: Maybe Double
  , stage   :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON RuleResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- GET /api/v2/rules

type RulesGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> QueryParam "enabled" Bool
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Bool
  :> Get '[JSON] [RuleResponse]

rulesGet ::
     AccessToken
  -> Maybe Bool
  -> Maybe Text
  -> Maybe Bool
  -> ClientM [RuleResponse]

--------------------------------------------------------------------------------
-- POST /api/v2/rules

data RuleCreate
  = RuleCreate
  { name    :: Maybe Text
  , script  :: Maybe Text
  , order   :: Maybe Double
  , enabled :: Maybe Bool
  } deriving (Generic, Show)

instance ToJSON RuleCreate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type RuleCreateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] RuleCreate
  :> Post '[JSON] RuleResponse

ruleCreate ::
     AccessToken
  -> RuleCreate
  -> ClientM RuleResponse

--------------------------------------------------------------------------------
-- GET /api/v2/rules/{id}

type RuleGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> QueryParam "fields" Text
  :> QueryParam "include_fields" Bool
  :> Get '[JSON] [RuleResponse]

ruleGet ::
     AccessToken
  -> Text
  -> Maybe Text
  -> Maybe Bool
  -> ClientM [RuleResponse]

--------------------------------------------------------------------------------
-- DELETE /api/v2/rules/{id}

type RuleDeleteApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Delete '[JSON] NoContent

ruleDelete ::
     AccessToken
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- PATCH /api/v2/rules/{id}

type RuleUpdate = RuleCreate

type RuleUpdateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> ReqBody '[JSON] RuleUpdate
  :> Patch '[JSON] RuleResponse

ruleUpdate ::
     AccessToken
  -> Text
  -> RuleUpdate
  -> ClientM RuleResponse

--------------------------------------------------------------------------------

type RulesApi
  =  "api"
  :> "v2"
  :> "rules"
  :> (    RulesGetApi
     :<|> RuleCreateApi
     :<|> RuleGetApi
     :<|> RuleDeleteApi
     :<|> RuleUpdateApi
     )

rulesApi :: Proxy RulesApi
rulesApi = Proxy

rulesGet
  :<|> ruleCreate
  :<|> ruleGet
  :<|> ruleDelete
  :<|> ruleUpdate
  = client rulesApi
