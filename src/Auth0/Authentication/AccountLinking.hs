module Auth0.Authentication.AccountLinking where

-- POST /api/v2/users/{id}/identities

runAccountLink :: IO ()
runAccountLink = error "Use Auth0.Management.Users.runUserLinkAccount instead"

-- DELETE /api/v2/users/{id}/identities/{provider}/{user_id}

runAccountUnlink :: IO ()
runAccountUnlink = error "Use Auth0.Management.Users.runDeleteUserIdentity instead"
