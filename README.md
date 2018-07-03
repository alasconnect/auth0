# Auth0 API in Haskell

Implements the following APIs:

- https://auth0.com/docs/api/authentication
- https://auth0.com/docs/api/management

# Usage

```haskell
-- import relevant module
import Auth0.Authentication.Signup

main :: IO ()
main = do
  -- example of logging in as a trusted client on behalf of the user
  let ath = mkAuth "https://my-tenant.auth0.com"
      pay = GetTokenResourceOwner Password
                                  (mkClientId "my-client-id")
                                  (Just (mkClientSecret "my-client-secret")
                                  (Just "my-audience")
                                  "username"
                                  "password"
                                  Nothing
                                  Nothing
  res <- runGetToken ath pay
  -- do things with the http status code, returned access token, auth0 error, etc.
```

# Testing

```bash
export AUTH0_TENANT=<tenant>
export AUTH0_CLIENT_ID=<client_id>
export AUTH0_CLIENT_SECRET=<client_secret>
export AUTH0_AUDIENCE=<audience>
export AUTH0_CONNECTION=<connection>

nix-shell --run 'cabal test --show-details=always --test-options=--color'
```

# Developing

```bash
nix-shell --run 'ghcid --command="cabal new-repl auth0"'
```
