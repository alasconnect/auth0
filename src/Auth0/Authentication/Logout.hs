module Auth0.Authentication.Logout where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Tagged (untag)
import Data.Text
---------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
---------------------------------------------------------------------------------

-- GET /v2/logout

data Logout
  = Logout
  { returnTo  :: Maybe Text
  , clientId  :: Maybe ClientId
  , federated :: Maybe Text
  }

instance ToRequest Logout where
  toRequest (Logout a b c) =
    [ ( "return_to", a )
    , ( "client_id", fmap untag b )
    , ( "federated", c )
    ]

runLogout
  :: (MonadIO m, MonadThrow m)
  => ByteString -> Logout -> m (Int, Maybe ())
runLogout h o =
  let api = API "GET" "/v2/logout"
  in execRequest h api o () Nothing
