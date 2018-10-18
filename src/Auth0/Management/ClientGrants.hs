module Auth0.Management.ClientGrants where

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

-- Response

data ClientGrantResponse
  = ClientGrantResponse
  { id       :: Maybe Text
  , clientId :: Maybe ClientId
  , audience :: Maybe Text
  , scope    :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance FromJSON ClientGrantResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- GET /api/v2/client-grants

type ClientGrantGetApi
  =  Header' '[Required] "Authorization" Text
  :> QueryParam "audience" Text
  :> QueryParam "client_id" ClientId
  :> Get '[JSON] [ClientGrantResponse]

clientGrantGet ::
     Text
  -> Maybe Text
  -> Maybe ClientId
  -> ClientM [ClientGrantResponse]

--------------------------------------------------------------------------------
-- POST /api/v2/client-grants

data ClientGrantCreate
  = ClientGrantsreate
  { clientId :: ClientId
  , audience :: Text
  , scope    :: (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON ClientGrantCreate where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type ClientGrantCreateApi
  =  Header' '[Required] "Authorization" Text
  :> ReqBody '[JSON] ClientGrantCreate
  :> Post '[JSON] NoContent

clientGrantCreate ::
     Text
  -> ClientGrantCreate
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- DELETE /api/v2/client-grants/{id}

type ClientGrantDeleteApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "client_id" ClientId
  :> Delete '[JSON] NoContent

clientGrantDelete ::
     Text
  -> ClientId
  -> ClientM NoContent

--------------------------------------------------------------------------------
-- PATCH /api/v2/client-grants/{id}

data ClientGrantUpdate
  = ClientGrantUpdate
  { scope    :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON ClientGrantUpdate

type ClientGrantUpdateApi
  =  Header' '[Required] "Authorization" Text
  :> Capture "client_id" ClientId
  :> ReqBody '[JSON] ClientGrantUpdate
  :> Put '[JSON] NoContent

clientGrantUpdate ::
     Text
  -> ClientId
  -> ClientGrantUpdate
  -> ClientM NoContent

--------------------------------------------------------------------------------

type ClientGrantApi
  =  "api"
  :> "v2"
  :> "client-grants"
  :> (    ClientGrantGetApi
     :<|> ClientGrantCreateApi
     :<|> ClientGrantDeleteApi
     :<|> ClientGrantUpdateApi
     )

clientGrantApi :: Proxy ClientGrantApi
clientGrantApi = Proxy

clientGrantGet
  :<|> clientGrantCreate
  :<|> clientGrantDelete
  :<|> clientGrantUpdate
  = client clientGrantApi
