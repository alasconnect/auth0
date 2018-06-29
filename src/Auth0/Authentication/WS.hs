module Auth0.Authentication.WS where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid ((<>))
import Data.Tagged
import Data.Text
import Data.Text.Encoding
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

-- WS - Federation
-- GET /wsfed/YOUR_CLIENT_ID

data WS
  = WS
  { wtrealm  :: Maybe Text
  , whr      :: Maybe Text
  , wctx     :: Maybe Text
  , wreply   :: Maybe Text
  } deriving (Show)

instance ToRequest WS where
  toRequest (WS a b c d) =
    [ toField "wtrealm" a
    , toField "whr" b
    , toField "wctx" c
    , toField "wreply" d
    ]

runWSFederation
  :: (MonadIO m, MonadThrow m)
  => Auth -> ClientId -> WS -> m (Auth0Response Text)
runWSFederation (Auth tenant) cid o =
  let api = API Get ("/wsfed/" <> (encodeUtf8 . untag) cid)
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) Nothing

-- GET /wsfed/YOUR_CLIENT_ID/FederationMetadata/2007-06/FederationMetadata.xml

runWSMetadata
  :: (MonadIO m, MonadThrow m)
  => Auth -> ClientId -> m (Auth0Response Text)
runWSMetadata (Auth tenant) cid =
  let xml = "/FederationMetadata/2007-06/FederationMetadata.xml"
      api = API Get ("/wsfed/" <> (encodeUtf8 . untag) cid <> xml)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing
