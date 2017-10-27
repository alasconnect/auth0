{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Authentication.AuthorizeClient where

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

data CodeGrant
  = CodeGrant
  { audience     :: Maybe Text
  , scope        :: Maybe Text
  , responseType :: ResponseType
  , clientId     :: ClientId
  , redirectUri  :: Maybe Text
  , state        :: Maybe Text
  , prompt       :: Maybe Text
  }

instance ToRequest CodeGrant where
  toRequest (CodeGrant a b c d e f g) =
    [ ( "audience", a )
    , ( "scope", b )
    , ( "response_type", (Just . pack . show . encode) c )
    , ( "client_id", (Just . untag) d )
    , ( "redirect_uri", e )
    , ( "state", f )
    , ( "prompt", g )
    ]

data CodeGrantPKCE
  = CodeGrantPKCE
  { audience            :: Maybe Text
  , scope               :: Maybe Text
  , responseType        :: ResponseType
  , clientId            :: ClientId
  , state               :: Maybe Text
  , redirectUri         :: Maybe Text
  , codeChallengeMethod :: Text
  , codeChallenge       :: Text
  , prompt              :: Maybe Text
  }

instance ToRequest CodeGrantPKCE where
  toRequest (CodeGrantPKCE a b c d e f g h i) =
    [ ( "audience", a )
    , ( "scope", b )
    , ( "response_type", (Just . pack . show . encode) c )
    , ( "client_id", (Just . untag) d )
    , ( "state", e )
    , ( "redirect_uri", f )
    , ( "code_challenge_method", Just g )
    , ( "code_challenge", Just h )
    , ( "prompt", i )
    ]

data CodeGrantImplicit
  = CodeGrantImplicit
  { audience     :: Maybe Text
  , scope        :: Maybe Text
  , responseType :: ResponseType
  , clientId     :: ClientId
  , state        :: Maybe Text
  , redirectUri  :: Maybe Text
  , nonce        :: Maybe Text
  , connection   :: Maybe Text
  , prompt       :: Maybe Text
  }

instance ToRequest CodeGrantImplicit where
  toRequest (CodeGrantImplicit a b c d e f g h i) =
    [ ( "audience", a )
    , ( "scope", b )
    , ( "response_type", (Just . pack . show . encode) c )
    , ( "client_id", (Just . untag) d )
    , ( "state", e )
    , ( "redirect_uri", f )
    , ( "nonce", g )
    , ( "connection", h )
    , ( "prompt", i )
    ]

runCodeGrant
  :: (MonadIO m, MonadThrow m, ToRequest a)
  => ByteString -> a -> m (Int, Maybe ())
runCodeGrant h o =
  let api = API "GET" "/authorize"
  in execRequest h api o () Nothing
