module Auth0.Management.Tickets where

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

data TicketResponse
  = TicketResponse
  { ticket :: Text
  } deriving (Generic, Show)

instance FromJSON TicketResponse where
  parseJSON =
    genericParseJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------------------------------------
-- POST /api/v2/tickets/email-verification

data TicketEmailVerification
  = TicketEmailVerification
  { resultUrl :: Maybe Text
  , userId    :: Text
  , ttlSec    :: Maybe Int
  } deriving (Generic, Show)

instance ToJSON TicketEmailVerification where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type TicketEmailVerificationCreateApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] TicketEmailVerification
  :> Post '[JSON] TicketResponse

ticketEmailVerifcationCreate ::
     AccessToken
  -> TicketEmailVerification
  -> ClientM TicketResponse

--------------------------------------------------------------------------------
-- POST /api/v2/tickets/password-change

data TicketChangePassword
  = TicketChangePassword
  { resultUrl    :: Maybe Text
  , userId       :: Maybe Text
  , newPassword  :: Maybe Text
  , connectionId :: Maybe Text
  , email        :: Maybe Text
  , ttlSec       :: Maybe Int
  } deriving (Generic, Show)

instance ToJSON TicketChangePassword where
  toJSON =
    genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' }

type TicketPasswordChangeApi
  =  Header' '[Required] "Authorization" AccessToken
  :> ReqBody '[JSON] TicketChangePassword
  :> Post '[JSON] TicketResponse

ticketPasswordChange ::
     AccessToken
  -> TicketChangePassword
  -> ClientM TicketResponse

--------------------------------------------------------------------------------

type TicketsApi
  =  "api"
  :> "v2"
  :> "tickets"
  :> (    TicketEmailVerificationCreateApi
     :<|> TicketPasswordChangeApi
     )

ticketsApi :: Proxy TicketsApi
ticketsApi = Proxy

ticketEmailVerifcationCreate
  :<|> ticketPasswordChange
  = client ticketsApi
