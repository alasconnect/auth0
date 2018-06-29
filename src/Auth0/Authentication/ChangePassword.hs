{-# LANGUAGE DeriveGeneric #-}

module Auth0.Authentication.ChangePassword where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Text
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

-- POST /dbconnections/change_password

data ChangePassword
  = ChangePassword
  { clientId   :: ClientId
  , email      :: Text
  , password   :: Maybe Text
  , connection :: Text
  } deriving (Generic, Show)

instance ToJSON ChangePassword where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runChangePassword
  :: (MonadIO m, MonadThrow m)
  => Auth -> ChangePassword -> m (Auth0Response ())
runChangePassword (Auth tenant) o =
  let api = API Post "/dbconnections/change_password"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) Nothing
