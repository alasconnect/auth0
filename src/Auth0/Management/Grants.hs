module Auth0.Management.Grants where

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

-- Response

data GrantResponse
  = GrantResponse
  { id       :: Text
  , clientId :: Text
  , userId   :: Text
  , audience :: Text
  , scope    :: [Text]
  } deriving (Generic, Show)

instance FromJSON GrantResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- GET /api/v2/grants

type GrantGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> QueryParam' '[Required] "user_id" Text
  :> Get '[JSON] [GrantResponse]

grantGet ::
     AccessToken
  -> Text
  -> ClientM [GrantResponse]

--------------------------------------------------------------------------------
-- DELETE /api/v2/grants/{id}

type GrantDeleteApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Delete '[JSON] NoContent

grantDelete ::
     AccessToken
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------

type GrantApi
  =  "api"
  :> "v2"
  :> "grants"
  :> (GrantGetApi :<|> GrantDeleteApi)

grantApi :: Proxy GrantApi
grantApi = Proxy

grantGet :<|> grantDelete = client grantApi
