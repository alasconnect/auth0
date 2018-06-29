{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Auth0.Management.RulesConfigs where

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
-- GET /api/v2/rules-configs

-- Response

data RuleConfigResponse
  = RuleConfigResponse
  { key :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON RuleConfigResponse

runGetRuleConfigs
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> m (Auth0Response [RuleConfigResponse])
runGetRuleConfigs (TokenAuth tenant accessToken) =
  let api = API Get "/api/v2/rules-configs"
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- DELETE /api/v2/rules-configs/{key}

runDeleteRuleConfig
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> m (Auth0Response ())
runDeleteRuleConfig (TokenAuth tenant accessToken) i =
  let api = API Delete ("/api/v2/rules-configs/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- PUT /api/v2/rules-configs/{key}

data RuleConfigSet
  = RuleConfigSet
  { value :: Text
  } deriving (Generic, Show)

instance ToJSON RuleConfigSet

data RuleConfigSetResponse
  = RuleConfigSetResponse
  { key   :: Text
  , value :: Text
  } deriving (Generic, Show)

instance FromJSON RuleConfigSetResponse

runSetRuleConfig
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> Text -> RuleConfigSet -> m (Auth0Response RuleConfigSetResponse)
runSetRuleConfig (TokenAuth tenant accessToken) i o =
  let api = API Put ("/api/v2/rules-configs/" <> encodeUtf8 i)
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])
