{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Authentication.SAML where

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

-- GET /samlp/YOUR_CLIENT_ID

data SAMLAcceptRequest
  = SAMLAcceptRequest
  { connection :: Maybe Text
  } deriving (Show)

instance ToRequest SAMLAcceptRequest where
  toRequest (SAMLAcceptRequest a) =
    [ ( "connection", a ) ]

runSAMLAcceptRequest
  :: (MonadIO m, MonadThrow m)
  => Auth -> ClientId -> SAMLAcceptRequest -> m (Auth0Response Text)
runSAMLAcceptRequest a cid o =
  let api = API Get ("/samlp/" <> (encodeUtf8 . untag) cid)
  in execRequest a api (Just o) (Nothing :: Maybe ()) Nothing

-- GET /samlp/metadata/YOUR_CLIENT_ID

runSAMLMetadata
  :: (MonadIO m, MonadThrow m)
  => Auth -> ClientId -> m (Auth0Response Text)
runSAMLMetadata a cid =
  let api = API Get ("/samlp/metadata/" <> (encodeUtf8 . untag) cid)
  in execRequest a api (Nothing :: Maybe ()) (Nothing :: Maybe ()) Nothing

-- IdP - Initiated SSO Flow
-- POST /login/callback

data IdP
  = IdP
  { connection   :: Text
  , samlResponse :: Text
  } deriving (Show)

instance ToRequest IdP where
  toRequest (IdP a b) =
    [ toField "connection" a
    , toField "saml_response" b
    ]

runSAMLIdP
  :: (MonadIO m, MonadThrow m)
  => Auth -> IdP -> m (Auth0Response Text)
runSAMLIdP a o =
  let api = API Post "/login/callback"
      hdr = [("Content-Type", "application/x-www-form-urlencoded")]
  in execRequest a api (Just o) (Nothing :: Maybe ()) (Just hdr)
