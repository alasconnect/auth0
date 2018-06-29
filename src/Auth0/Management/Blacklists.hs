{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Auth0.Management.Blacklists where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Text
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/blacklists/tokens

-- Request

data BlacklistToken
  = BlacklistToken
  { aud :: Maybe Text
  } deriving (Show)

instance ToRequest BlacklistToken where
  toRequest (BlacklistToken a) =
    [ toField "aud" a
    ]

-- Request / Response

data BlacklistTokenResponse
  = BlacklistTokenResponse
  { aud :: Maybe Text
  , jti :: Text
  } deriving (Generic, Show)

instance FromJSON BlacklistTokenResponse

runGetBlacklistTokens
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> BlacklistToken -> m (Auth0Response [BlacklistTokenResponse])
runGetBlacklistTokens (TokenAuth tenant accessToken) o =
  let api = API Get "/api/v2/blacklist/tokens"
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- POST /api/v2/blacklists/tokens

type BlacklistTokenDo = BlacklistTokenResponse

instance ToJSON BlacklistTokenDo

runBlacklistToken
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> BlacklistTokenDo -> m (Auth0Response ())
runBlacklistToken (TokenAuth tenant accessToken) o =
  let api = API Post "/api/v2/blacklist/tokens"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])
