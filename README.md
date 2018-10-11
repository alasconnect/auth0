# Auth0 API in Haskell

Implements the following APIs:

- https://auth0.com/docs/api/authentication
- https://auth0.com/docs/api/management

# Note

This API is not fully tested due to the large number of Auth0 features. Please send a PR if you find that a particular endpoint doesn't behave as expected.

# Usage

```haskell
-- import relevant module
import Auth0.Authentication.GetToken

main :: IO ()
main = do
  -- example of fetching client credentials
  let bdy = GetTokenClientCreds ClientCredentials "client_id" "secret" "https://my-tenant.auth0.com/api/v2/"
  env <- mkAuth0Env "my-tenant.auth0.com"
  res <- runClientM (getTokenClientCreds bdy) env
```

# Testing

```bash
export AUTH0_TENANT=<tenant>
export AUTH0_CLIENT_ID=<client_id>
export AUTH0_CLIENT_SECRET=<client_secret>
export AUTH0_AUDIENCE=<audience>
export AUTH0_CONNECTION=<connection>

nix-shell --run 'cabal new-test --enable-tests'
```

# Developing

```bash
nix-shell --run 'ghcid --command="cabal new-repl auth0"'
```
