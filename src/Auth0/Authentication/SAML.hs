{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Authentication.SAML where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid ((<>))
import Data.Tagged
import Data.Text
import Data.Text.Encoding
---------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
---------------------------------------------------------------------------------

-- GET /samlp/YOUR_CLIENT_ID

data SAMLAcceptRequest
  = SAMLAcceptRequest
  { connection :: Maybe Text
  }

instance ToRequest SAMLAcceptRequest where
  toRequest (SAMLAcceptRequest a) =
    [ ( "connection", a ) ]

runSAMLAcceptRequest
  :: (MonadIO m, MonadThrow m)
  => Host -> ClientId -> SAMLAcceptRequest -> m (Int, Maybe Text)
runSAMLAcceptRequest h cid o =
  let api = API Get ("/samlp/" <> (encodeUtf8 . untag) cid)
  in execRequest h api o () Nothing

-- GET /samlp/metadata/YOUR_CLIENT_ID

runSAMLMetadata
  :: (MonadIO m, MonadThrow m)
  => Host -> ClientId -> m (Int, Maybe Text)
runSAMLMetadata h cid =
  let api = API Get ("/samlp/metadata/" <> (encodeUtf8 . untag) cid)
  in execRequest h api () () Nothing

-- IdP - Initiated SSO Flow
-- POST /login/callback

data IdP
  = IdP
  { connection   :: Text
  , samlResponse :: Text
  }

instance ToRequest IdP where
  toRequest (IdP a b) =
    [ toField "connection" a
    , toField "saml_response" b
    ]

runSAMLIdP
  :: (MonadIO m, MonadThrow m)
  => Host -> IdP -> m (Int, Maybe Text)
runSAMLIdP h o =
  let api = API Post "/login/callback"
      hdr = [("Content-Type", "application/x-www-form-urlencoded")]
  in execRequest h api o () (Just hdr)
