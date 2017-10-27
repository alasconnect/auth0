module Auth0.Authentication.WS where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid ((<>))
import Data.Tagged
import Data.Text
import Data.Text.Encoding
import Data.ByteString (ByteString)
---------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
---------------------------------------------------------------------------------

-- WS - Federation
-- GET /wsfed/YOUR_CLIENT_ID

data WS
  = WS
  { clientId :: ClientId
  , wtrealm  :: Maybe Text
  , whr      :: Maybe Text
  , wctx     :: Maybe Text
  , wreply   :: Maybe Text
  }

instance ToRequest WS where
  toRequest (WS a b c d e) =
    [ ( "client-id", (Just . untag) a )
    , ( "wtrealm", b )
    , ( "whr", c )
    , ( "wctx", d )
    , ( "wreply", e )
    ]

runWSFederation
  :: (MonadIO m, MonadThrow m)
  => ByteString -> ClientId -> WS -> m (Int, Maybe Text)
runWSFederation h cid o =
  let api = API "GET" ("/wsfed/" <> (encodeUtf8 . untag) cid)
  in execRequest h api o () Nothing

-- GET /wsfed/YOUR_CLIENT_ID/FederationMetadata/2007-06/FederationMetadata.xml

runWSMetadata
  :: (MonadIO m, MonadThrow m)
  => ByteString -> ClientId -> m (Int, Maybe Text)
runWSMetadata h cid =
  let xml = "/FederationMetadata/2007-06/FederationMetadata.xml"
      api = API "GET" ("/wsfed/" <> (encodeUtf8 . untag) cid <> xml)
  in execRequest h api () () Nothing
