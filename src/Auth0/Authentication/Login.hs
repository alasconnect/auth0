module Auth0.Authentication.Login where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Map
import Data.Text
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

-- GET /authorize

data Login
  = Login
  { responseType         :: ResponseType
  , clientId             :: ClientId
  , connection           :: Maybe Text
  , redirectUri          :: Text
  , state                :: Maybe Text
  , additionalParameters :: Maybe (Map Text Text)
  } deriving (Show)

instance ToRequest Login where
  toRequest (Login a b c d e f) =
    [ toField "response_type" a
    , toField "client_id" b
    , toField "connection" c
    , toField "redirect_uri" d
    , toField "state" e
    ] ++ case f of
           Nothing -> []
           Just f' -> fmap (\(k, v) -> toField k (Just v)) (toList f')

runLogin
  :: (MonadIO m, MonadThrow m)
  => Auth -> Login -> m (Auth0Response ())
runLogin (Auth tenant) o =
  let api = API Get "/authorize"
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) Nothing
