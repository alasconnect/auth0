{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Auth0.Management.Blacklists where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Text
import GHC.Generics
---------------------------------------------------------------------------------
import Auth0.Request
---------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- GET /api/v2/blacklists/tokens

-- Request

data BlacklistToken
  = BlacklistToken
  { aud :: Maybe Text
  }

instance ToRequest BlacklistToken where
  toRequest (BlacklistToken a) =
    [ toField "aud" a
    ]

-- Request / Response

data BlacklistTokenResponse
  = BlacklistTokenResponse
  { aud :: Maybe Text
  , jti :: Text
  } deriving (Generic)

instance FromJSON BlacklistTokenResponse

runGetBlacklistTokens
  :: (MonadIO m, MonadThrow m)
  => Host -> BlacklistToken -> m (Int, Maybe [BlacklistTokenResponse])
runGetBlacklistTokens h o =
  let api = API Get "/api/v2/blacklist/tokens"
  in execRequest h api o () Nothing

---------------------------------------------------------------------------------
-- POST /api/v2/blacklists/tokens

type BlacklistTokenDo = BlacklistTokenResponse

instance ToJSON BlacklistTokenDo

runBlacklistToken
  :: (MonadIO m, MonadThrow m)
  => Host -> BlacklistTokenDo -> m (Int, Maybe ())
runBlacklistToken h o =
  let api = API Post "/api/v2/blacklist/tokens"
  in execRequest h api () o Nothing
