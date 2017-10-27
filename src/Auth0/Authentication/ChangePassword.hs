{-# LANGUAGE DeriveGeneric #-}

module Auth0.Authentication.ChangePassword where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Text
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
---------------------------------------------------------------------------------

-- POST /dbconnections/change_password

data ChangePassword
  = ChangePassword
  { clientId     :: ClientId
  , email        :: Text
  , password     :: Maybe Text
  , connection   :: Text
  } deriving (Generic)

instance ToJSON ChangePassword where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runChangePassword
  :: (MonadIO m, MonadThrow m)
  => ByteString -> ChangePassword -> m (Int, Maybe ())
runChangePassword h o =
  let api = API "POST" "/dbconnections/change_password"
  in execRequest h api () (Just o) Nothing
