module Auth0.Request where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Bifunctor
import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Simple
import Network.HTTP.Types.Header
---------------------------------------------------------------------------------

data API = API ByteString ByteString

-- | 'ToRequest' convert a record to a list of tuple pairs.
class ToRequest a where
  toRequest :: a -> [(Text, Maybe Text)]

  buildRequest :: a -> [(ByteString, Maybe ByteString)]
  buildRequest = fmap (bimap encodeUtf8 (fmap encodeUtf8)) . toRequest

instance ToRequest () where
  toRequest _ = [] :: [(Text, Maybe Text)]

execRequest
  :: (MonadIO m, MonadThrow m, ToRequest a, ToJSON b, FromJSON c)
  => ByteString                       -- ^ Host URL
  -> API                              -- ^ API call - (API "POST" "/api")
  -> a                                -- ^ Optional request query
  -> b                                -- ^ Optional JSON payload
  -> Maybe [(HeaderName, ByteString)] -- ^ Optional headers
  -> m (Int, Maybe c)                 -- ^ Status code, and possible return type
execRequest h (API m p) o j hs = do
  let req = setRequestMethod m
          . setRequestHost h
          . setRequestPath p
          . (setRequestQueryString . buildRequest) o
          . setRequestBodyJSON j
          . (\r -> maybe r (\v -> setRequestHeaders v r) hs)
          $ defaultRequest
  httpLBS req >>= \res ->
    return (getResponseStatusCode res, decode (getResponseBody res))
