{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.RulesConfigs where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- GET /api/v2/rules-configs

-- Response

data RuleConfigResponse
  = RuleConfigResponse
  { key :: Maybe Text
  } deriving (Generic)

instance FromJSON RuleConfigResponse

runGetRuleConfigs
  :: (MonadIO m, MonadThrow m)
  => Host -> m (Int, Maybe [RuleConfigResponse])
runGetRuleConfigs h =
  let api = API Get "/api/v2/rules-configs"
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- DELETE /api/v2/rules-configs/{key}

runDeleteRuleConfig
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> m (Int, Maybe ())
runDeleteRuleConfig h i =
  let api = API Delete ("/api/v2/rules-configs/" <> encodeUtf8 i)
  in execRequest h api () () Nothing

---------------------------------------------------------------------------------
-- PUT /api/v2/rules-configs/{key}

data RuleConfigSet
  = RuleConfigSet
  { value :: Text
  } deriving (Generic)

instance ToJSON RuleConfigSet

data RuleConfigSetResponse
  = RuleConfigSetResponse
  { key   :: Text
  , value :: Text
  } deriving (Generic)

instance FromJSON RuleConfigSetResponse

runSetRuleConfig
  :: (MonadIO m, MonadThrow m)
  => Host -> Text -> RuleConfigSet -> m (Int, Maybe RuleConfigSetResponse)
runSetRuleConfig h i o =
  let api = API Put ("/api/v2/rules-configs/" <> encodeUtf8 i)
  in execRequest h api () o Nothing
