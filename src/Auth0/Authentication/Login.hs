module Auth0.Authentication.Login where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Text
---------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
---------------------------------------------------------------------------------

-- GET /authorize

data Login
  = Login
  { responseType        :: ResponseType
  , clientId            :: ClientId
  , connection          :: Maybe Text
  , redirectUri         :: Text
  , state               :: Maybe Text
  , additionalParameter :: Text
  }

instance ToRequest Login where
  toRequest (Login a b c d e f) =
    [ toField "response_type" a
    , toField "client_id" b
    , toField "connection" c
    , toField "redirect_uri" d
    , toField "state" e
    , toField "additional-parameter" f
    ]

runLogin
  :: (MonadIO m, MonadThrow m)
  => Host -> Login -> m (Int, Maybe ())
runLogin h o =
  let api = API Get "/authorize"
  in execRequest h api o () Nothing
