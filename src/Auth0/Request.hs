{-# LANGUAGE FlexibleInstances #-}

module Auth0.Request where

---------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Map
import Data.Monoid ((<>))
import Data.Tagged
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Simple
import Network.HTTP.Types.Header
---------------------------------------------------------------------------------

data HostTag
type Host = Tagged HostTag ByteString

mkHost :: ByteString -> Host
mkHost = Tagged

data Verb
  = Get
  | Post
  | Put
  | Update
  | Delete

instance Show Verb where
  show Get    = "GET"
  show Post   = "POST"
  show Put    = "PUT"
  show Update = "PATCH"
  show Delete = "DELETE"

data API = API Verb ByteString

class ToField a where
  toField :: Text -> a -> (Text, Maybe Text)

instance ToField Bool where
  toField t v  = (t, (Just . toLower . pack . show) v)

instance ToField Int where
  toField t v = (t, (Just . pack. show) v)

instance ToField Text where
  toField t v = (t, Just v)

instance ToField a => ToField (Maybe a) where
  toField t Nothing = (t, Nothing)
  toField t (Just v) = toField t v

-- | 'ToRequest' converts a record to a list of tuples.
class ToRequest a where
  toRequest :: a -> [(Text, Maybe Text)]

  buildRequest :: a -> [(ByteString, Maybe ByteString)]
  buildRequest = fmap (bimap encodeUtf8 (fmap encodeUtf8)) . toRequest

instance ToRequest () where
  toRequest _ = [] :: [(Text, Maybe Text)]

flattenMap :: Text -> Map Text Text -> [(Text, Maybe Text)]
flattenMap t = fmap (\(k, v) -> (t <> "[" <> k <> "]", Just v)) . toList

execRequest
  :: (MonadIO m, MonadThrow m, ToRequest a, ToJSON b, FromJSON c)
  => Host                             -- ^ Host URL
  -> API                              -- ^ API call - (API "POST" "/api")
  -> a                                -- ^ Optional request query
  -> b                                -- ^ Optional JSON payload
  -> Maybe [(HeaderName, ByteString)] -- ^ Optional headers
  -> m (Int, Maybe c)                 -- ^ Status code, and possible return type
execRequest h (API m p) o j hs = do
  let req = (setRequestMethod . encodeUtf8 . pack . show) m
          . (setRequestHost . untag) h
          . setRequestPath p
          . (setRequestQueryString . buildRequest) o
          . setRequestBodyJSON j
          . (\r -> maybe r (\v -> setRequestHeaders v r) hs)
          $ defaultRequest
  httpLBS req >>= \res ->
    return (getResponseStatusCode res, decode (getResponseBody res))
