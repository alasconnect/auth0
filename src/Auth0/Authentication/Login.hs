module Auth0.Authentication.Login where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.Tagged (untag)
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
    [ ( "response_type", (Just . pack . show . encode) a )
    , ( "client_id", (Just . untag) b )
    , ( "connection", c )
    , ( "redirect_uri", Just d )
    , ( "state", e )
    , ( "additional-parameter", Just f )
    ]

runLogin
  :: (MonadIO m, MonadThrow m)
  => ByteString -> Login -> m (Int, Maybe ())
runLogin h o =
  let api = API "GET" "/authorize"
  in execRequest h api o () Nothing
