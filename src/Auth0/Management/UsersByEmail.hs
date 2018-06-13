module Auth0.Management.UsersByEmail where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import Data.Text
--------------------------------------------------------------------------------
import Auth0.Management.Users (UserResponse)
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/users-by-email

-- Request

data UsersByEmail
  = UsersByEmail
  { fields        :: Maybe Text
  , includeFields :: Maybe Text
  , email         :: Text
  } deriving (Show)

instance ToRequest UsersByEmail where
  toRequest (UsersByEmail a b c) =
    [ toField "fields" a
    , toField "include_fields" b
    , toField "email" c
    ]

runGetUsersByEmail
  :: (MonadIO m, MonadThrow m, FromJSON appMd, FromJSON userMd)
  => Auth -> UsersByEmail -> m (Auth0Response [UserResponse appMd userMd])
runGetUsersByEmail a o =
  let api = API Get "/api/v2/users-by-email"
  in execRequest a api (Just o) (Nothing :: Maybe ()) Nothing
