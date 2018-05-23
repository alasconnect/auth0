{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Tickets where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Text
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- POST /api/v2/tickets/email-verification

-- Request

data TicketEmailVerification
  = TicketEmailVerification
  { resultUrl :: Maybe Text
  , userId    :: Text
  , ttlSec    :: Maybe Int
  } deriving (Generic, Show)

instance ToJSON TicketEmailVerification where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- Response

data TicketResponse
  = TicketResponse
  { ticket :: Text
  } deriving (Generic, Show)

instance FromJSON TicketResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateTicketEmailVerification
  :: (MonadIO m, MonadThrow m)
  => Auth -> TicketEmailVerification -> m (Auth0Response TicketResponse)
runCreateTicketEmailVerification a o =
  let api = API Post "/api/v2/tickets/email-verification"
  in execRequest a api (Nothing :: Maybe ()) (Just o) Nothing

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
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateTicketChangePassword
  :: (MonadIO m, MonadThrow m)
  => Auth -> TicketChangePassword -> m (Auth0Response TicketResponse)
runCreateTicketChangePassword a o =
  let api = API Post "/api/v2/tickets/password-change"
  in execRequest a api (Nothing :: Maybe ()) (Just o) Nothing
