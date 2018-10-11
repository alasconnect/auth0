module Auth0.Management.Blacklists where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client
--------------------------------------------------------------------------------

-- Response

data BlacklistTokenResponse
  = BlacklistTokenResponse
  { aud :: Maybe Text
  , jti :: Text
  } deriving (Generic, Show)

instance FromJSON BlacklistTokenResponse

--------------------------------------------------------------------------------
-- GET /api/v2/blacklists/tokens

type BlacklistTokenGetApi
  =  Header' '[Required] "Authorization" Text
  :> QueryParam "aud" Text
  :> Get '[JSON] [BlacklistTokenResponse]

blacklistTokenGet ::
     Text
  -> Maybe Text
  -> ClientM [BlacklistTokenResponse]

--------------------------------------------------------------------------------
-- POST /api/v2/blacklists/tokens

type BlacklistTokenDo = BlacklistTokenResponse

instance ToJSON BlacklistTokenDo

type BlacklistTokenPostApi
  =  Header' '[Required] "Authorization" Text
  :> ReqBody '[JSON] BlacklistTokenDo
  :> Post '[JSON] NoContent

blacklistTokenPost ::
     Text
  -> BlacklistTokenDo
  -> ClientM NoContent

--------------------------------------------------------------------------------

type BlacklistApi
  =  "api"
  :> "v2"
  :> "blacklist"
  :> "tokens"
  :> (BlacklistTokenGetApi :<|> BlacklistTokenPostApi)

blacklistApi :: Proxy BlacklistApi
blacklistApi = Proxy

blacklistTokenGet :<|> blacklistTokenPost = client blacklistApi
