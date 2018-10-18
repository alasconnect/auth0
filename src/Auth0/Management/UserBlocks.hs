module Auth0.Management.UserBlocks where

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

data BlockedFor
  = BlockedFor
  { identifier :: Text
  , ip         :: Text
  } deriving (Generic, Show)

instance FromJSON BlockedFor

data UserBlockResponse
  = UserBlockResponse
  { blockedFor :: Maybe [BlockedFor]
  } deriving (Generic, Show)

instance FromJSON UserBlockResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- GET /api/v2/user-blocks

type UserBlocksGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> QueryParam' '[Required] "identifier" Text
  :> Get '[JSON] UserBlockResponse

userBlocksGet ::
     AccessToken
  -> Text
  -> ClientM UserBlockResponse

--------------------------------------------------------------------------------
-- DELETE /api/v2/user-blocks

type UserBlocksDeleteApi
  =  Header' '[Required] "Authorization" AccessToken
  :> QueryParam' '[Required] "identifier" Text
  :> Delete '[JSON] NoContent

userBlocksDelete ::
     AccessToken
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- GET /api/v2/user-blocks/{id}

type UserBlockGetApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Get '[JSON] UserBlockResponse

userBlockGet ::
     AccessToken
  -> Text
  -> ClientM UserBlockResponse

--------------------------------------------------------------------------------
-- DELETE /api/v2/user-blocks/{id}

type UserBlockDeleteApi
  =  Header' '[Required] "Authorization" AccessToken
  :> Capture "id" Text
  :> Delete '[JSON] NoContent

userBlockDelete ::
     AccessToken
  -> Text
  -> ClientM NoContent

--------------------------------------------------------------------------------

type UserBlocksApi
  =  "api"
  :> "v2"
  :> "user-blocks"
  :> (    UserBlocksGetApi
     :<|> UserBlocksDeleteApi
     :<|> UserBlockGetApi
     :<|> UserBlocksDeleteApi
     )

userBlocksApi :: Proxy UserBlocksApi
userBlocksApi = Proxy

userBlocksGet
  :<|> userBlocksDelete
  :<|> userBlockGet
  :<|> userBlockDelete
  = client userBlocksApi
