{-# LANGUAGE DeriveGeneric #-}

module Auth0.Authentication.UserProfile where

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

-- GET /userinfo

data UserProfileResponse
  = UserProfileResponse
  { emailVerified :: Bool
  , email         :: Text
  , clientID      :: Text
  , updatedAt     :: Text
  , name          :: Text
  , picture       :: Text
  , userId        :: Text
  , nickname      :: Text
  , createdAt     :: Text
  , sub           :: Text
  } deriving (Generic, Show)

instance FromJSON UserProfileResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runUserProfile
  :: (MonadIO m, MonadThrow m)
  => Auth -> m (Auth0Response UserProfileResponse)
runUserProfile (Auth tenant) =
  let api = API Get "/userinfo"
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing
