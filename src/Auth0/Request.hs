{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth0.Request where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Simple
import Network.HTTP.Types.Header (HeaderName)
--------------------------------------------------------------------------------
import Auth0.Types
--------------------------------------------------------------------------------

-- | Convenience function for flattening URL params into foo[bar] syntax.
flattenMap :: Text -> Map Text Text -> [(Text, Maybe Text)]
flattenMap t = fmap (\(k, v) -> (t <> "[" <> k <> "]", Just v)) . toList

-- | Execute a request against an API endpoint.
execRequest
  :: forall m a b c. (MonadIO m, MonadThrow m, ToRequest a, ToJSON b, FromJSON c, Show b)
  => Tenant                           -- ^ Tenant to connect to
  -> API                              -- ^ API call - (API "POST" "/api")
  -> Maybe a                          -- ^ Optional request query
  -> Maybe b                          -- ^ Optional JSON payload
  -> Maybe [(HeaderName, ByteString)] -- ^ Optional headers
  -> m (Auth0Response c)              -- ^ Response
execRequest t (API m p) a b hs = do
  let req = (setRequestMethod . encodeUtf8 . T.pack . show) m
          . (setRequestHost . untag) t
          . setRequestPort 443
          . setRequestSecure True
          . setRequestPath p
          . maybe id (setRequestQueryString . buildRequest) a
          . setRequestBodyJSON b
          . setRequestHeaders (fromMaybe [] hs)
          $ defaultRequest
  httpLBS req >>= \res ->
    let code = getResponseStatusCode res
        hdrs = fromList (getResponseHeaders res)
        resp = Auth0Response code Nothing hdrs req Nothing
        body = getResponseBody res
    in case code of
      200 ->
        case eitherDecode body :: Either String c of
          Left _ -> return resp
          Right response -> return $ resp { resPayload = Just response }
      _   ->
        case eitherDecode body :: Either String Auth0Error of
          Left _ -> return resp
          Right err'     -> return $ resp { resError = Just err' }
