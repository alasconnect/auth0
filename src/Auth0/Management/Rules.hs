{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Rules where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/rules

-- Request

data Rule
  = Rule
  { enabled       :: Maybe Bool
  , fields        :: Maybe Text
  , includeFields :: Maybe Bool
  } deriving (Show)

instance ToRequest Rule where
  toRequest (Rule a b c) =
    [ toField "enabled" a
    , toField "fields" b
    , toField "include_fields" c
    ]

-- Response

data RuleResponse
  = RuleResponse
  { name    :: Maybe Text
  , id      :: Maybe Text
  , enabled :: Maybe Bool
  , script  :: Maybe Text
  , number  :: Maybe Double
  , stage   :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON RuleResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetRules
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Rule -> m (Auth0Response [RuleResponse])
runGetRules (TokenAuth tenant accessToken) o =
  let api = API Get "/api/v2/rules"
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/rules

data RuleCreate
  = RuleCreate
  { name    :: Maybe Text
  , script  :: Maybe Text
  , order   :: Maybe Double
  , enabled :: Maybe Bool
  } deriving (Generic, Show)

instance ToJSON RuleCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateRule
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> RuleCreate -> m (Auth0Response RuleResponse)
runCreateRule (TokenAuth tenant accessToken) o =
  let api = API Post "/api/v2/rules"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- GET /api/v2/rules/{id}

-- Request

data RuleGet
  = RuleGet
  { fields        :: Maybe Text
  , includeFields :: Maybe Bool
  } deriving (Show)

instance ToRequest RuleGet where
  toRequest (RuleGet a b) =
    [ toField "fields" a
    , toField "include_fields" b
    ]

runGetRule
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> RuleGet -> m (Auth0Response RuleResponse)
runGetRule (TokenAuth tenant accessToken) i o =
  let api = API Get ("/api/v2/rules/" <> encodeUtf8 i)
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- DELETE /api/v2/rules/{id}

runDeleteRule
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response ())
runDeleteRule (TokenAuth tenant accessToken) i =
  let api = API Delete ("/api/v2/rules/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- PATCH /api/v2/rules/{id}

type RuleUpdate = RuleCreate

runUpdateRule
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> RuleUpdate -> m (Auth0Response RuleResponse)
runUpdateRule (TokenAuth tenant accessToken) i o =
  let api = API Update ("/api/v2/rules/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])
