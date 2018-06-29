{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Authentication.Signup where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Map
import Data.Text
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

-- POST /dbconnections/signup

data Signup
  = Signup
  { clientId     :: ClientId
  , email        :: Text
  , password     :: Text
  , connection   :: Text
  , userMetadata :: Maybe (Map Text Text)
  } deriving (Generic, Show)

instance ToJSON Signup where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data SignupResponse
  = SignupResponse
  { _id           :: Text
  , emailVerified :: Bool
  , email         :: Text
  } deriving (Generic, Show)

instance FromJSON SignupResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runSignup
  :: (MonadIO m, MonadThrow m)
  => Auth -> Signup -> m (Auth0Response SignupResponse)
runSignup (Auth tenant) o =
  let api = API Post "/dbconnections/signup"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) Nothing
