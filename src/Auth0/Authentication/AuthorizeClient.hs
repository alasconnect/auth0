{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Authentication.AuthorizeClient where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Text
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

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
  } deriving (Show)

instance ToRequest CodeGrant where
  toRequest (CodeGrant a b c d e f g) =
    [ toField "audience" a
    , toField "scope" b
    , toField "response_type" c
    , toField "client_id" d
    , toField "redirect_uri" e
    , toField "state" f
    , toField "prompt" g
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
  } deriving (Show)

instance ToRequest CodeGrantPKCE where
  toRequest (CodeGrantPKCE a b c d e f g h i) =
    [ toField "audience" a
    , toField "scope" b
    , toField "response_type" c
    , toField "client_id" d
    , toField "state" e
    , toField "redirect_uri" f
    , toField "code_challenge_method" g
    , toField "code_challenge" h
    , toField "prompt" i
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
  } deriving (Show)

instance ToRequest CodeGrantImplicit where
  toRequest (CodeGrantImplicit a b c d e f g h i) =
    [ toField "audience" a
    , toField "scope" b
    , toField "response_type" c
    , toField "client_id" d
    , toField "state" e
    , toField "redirect_uri" f
    , toField "nonce" g
    , toField "connection" h
    , toField "prompt" i
    ]

runGetCodeGrant
  :: (MonadIO m, MonadThrow m, ToRequest a)
  => Auth -> a -> m (Auth0Response ())
runGetCodeGrant (Auth tenant) o =
  let api = API Get "/authorize"
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) Nothing
