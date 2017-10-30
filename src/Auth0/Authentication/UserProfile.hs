{-# LANGUAGE DeriveGeneric #-}

module Auth0.Authentication.UserProfile where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
---------------------------------------------------------------------------------

-- GET /userinfo

data UserProfile
  = UserProfile
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
  } deriving (Generic)

instance FromJSON UserProfile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runUserProfile
  :: (MonadIO m, MonadThrow m)
  => Host -> m (Int, Maybe UserProfile)
runUserProfile h =
  let api = API Get "/userinfo"
      hdr = [("Authorization", "Bearer ")]
  in execRequest h api () () (Just hdr)
