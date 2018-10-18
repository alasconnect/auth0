module Auth0 where

--------------------------------------------------------------------------------
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client
--------------------------------------------------------------------------------

-- Create a ClientEnv for servant-client's runClientM
mkAuth0Env :: String -> IO ClientEnv
mkAuth0Env tnt = do
  m <- newManager tlsManagerSettings
  return (mkClientEnv m (BaseUrl Https tnt 443 ""))
