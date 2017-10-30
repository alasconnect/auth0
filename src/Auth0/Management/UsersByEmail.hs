module Auth0.Management.UsersByEmail where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Text
---------------------------------------------------------------------------------
import Auth0.Management.Users (UserResponse)
import Auth0.Request
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- GET /api/v2/users-by-email

-- Request

data UsersByEmail
  = UsersByEmail
  { fields        :: Maybe Text
  , includeFields :: Maybe Text
  , email         :: Text
  }

instance ToRequest UsersByEmail where
  toRequest (UsersByEmail a b c) =
    [ toField "fields" a
    , toField "include_fields" b
    , toField "email" c
    ]

runGetUsersByEmail
  :: (MonadIO m, MonadThrow m)
  => Host -> UsersByEmail -> m (Int, Maybe [UserResponse])
runGetUsersByEmail h o =
  let api = API Get "/api/v2/users-by-email"
  in execRequest h api o () Nothing
