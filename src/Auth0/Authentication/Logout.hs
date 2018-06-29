module Auth0.Authentication.Logout where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Text
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

-- GET /v2/logout

data Logout
  = Logout
  { returnTo  :: Maybe Text
  , clientId  :: Maybe ClientId
  , federated :: Maybe Text
  } deriving (Show)

instance ToRequest Logout where
  toRequest (Logout a b c) =
    [ toField "return_to" a
    , toField "client_id" b
    , toField "federated" c
    ]

runLogout
  :: (MonadIO m, MonadThrow m)
  => Auth -> Maybe Logout -> m (Auth0Response ())
runLogout (Auth tenant) o =
  let api = API Get "/v2/logout"
  in execRequest tenant api o (Nothing :: Maybe ()) Nothing
