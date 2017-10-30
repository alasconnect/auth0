{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.Rules where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- GET /api/v2/rules

-- Request

data Rule
  = Rule
  { enabled       :: Maybe Bool
  , fields        :: Maybe Text
  , includeFields :: Maybe Bool
  }

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
  } deriving (Generic)

instance FromJSON RuleResponse where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runGetRules
  :: (MonadIO m, MonadThrow m)
  => Host -> Rule -> m (Int, Maybe [RuleResponse])
runGetRules h o =
  let api = API Get "/api/v2/rules"
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- POST /api/v2/rules

data RuleCreate
  = RuleCreate
  { name    :: Maybe Text
  , script  :: Maybe Text
  , order   :: Maybe Double
  , enabled :: Maybe Bool
  } deriving (Generic)

instance ToJSON RuleCreate where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runCreateRule
  :: (MonadIO m, MonadThrow m)
  => Host -> RuleCreate -> m (Int, Maybe RuleResponse)
runCreateRule h o =
  let api = API Post "/api/v2/rules"
  in execRequest h api () o Nothing

---------------------------------------------------------------------------------
-- GET /api/v2/rules/{id}

-- Request

data RuleGet
  = RuleGet
  { fields        :: Maybe Text
  , includeFields :: Maybe Bool
  }

instance ToRequest RuleGet where
  toRequest (RuleGet a b) =
    [ toField "fields" a
    , toField "include_fields" b
    ]

runGetRule
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> RuleGet -> m (Int, Maybe RuleResponse)
runGetRule h i o =
  let api = API Get ("/api/v2/rules/" <> encodeUtf8 i)
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/rules/{id}

runDeleteRule
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe ())
runDeleteRule h i =
  let api = API Delete ("/api/v2/rules/" <> encodeUtf8 i)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- PATCH /api/v2/rules/{id}

type RuleUpdate = RuleCreate

runUpdateRule
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> RuleUpdate -> m (Int, Maybe RuleResponse)
runUpdateRule h i o =
  let api = API Update ("/api/v2/rules/" <> encodeUtf8 i)
  in execRequest h api () o Nothing
