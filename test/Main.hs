{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Either
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Servant.Client
import System.Environment (lookupEnv)
import System.Random
--------------------------------------------------------------------------------
import Test.Hspec
--------------------------------------------------------------------------------
import Auth0
import Auth0.Authentication.GetToken
import Auth0.Authentication.Signup
import Auth0.Management.Users
import Auth0.Types
--------------------------------------------------------------------------------

main :: IO ()
main = do
  tnt <- lookupEnv "AUTH0_TENANT" >>= return . fromMaybe ""
  cid <- lookupEnv "AUTH0_CLIENT_ID" >>= \case
    Nothing -> return ""
    Just v  -> return . mkClientId . pack $ v
  cst <- lookupEnv "AUTH0_CLIENT_SECRET" >>= \case
    Nothing -> return ""
    Just v  -> return . mkClientSecret . pack $ v
  aud <- lookupEnv "AUTH0_AUDIENCE" >>= \case
    Nothing -> return ""
    Just v  -> return . pack $ v
  con <- lookupEnv "AUTH0_CONNECTION" >>= \case
    Nothing -> return ""
    Just v  -> return . pack $ v

  g <- getStdGen
  let usr = (pack . take 32) (randomRs ('a', 'z') g) <> "@example.com"
      pwd = "TestAuth0!" :: Text

  hspec $ do
    describe "Authentication" $ do
      describe "GetTokenClientCreds" $
        it "Get Token Client Credentials" $ do
          let bdy = GetTokenClientCreds ClientCredentials cid cst aud
          env <- mkAuth0Env tnt
          res <- runClientM (getTokenClientCreds bdy) env
          isRight res `shouldBe` True
          case res of
            Left _  -> return ()
            Right v -> tokenType v `shouldBe` "Bearer"

      describe "Signup" $
        it "Signup" $ do
          let pay = Signup cid usr pwd con Nothing
          env <- mkAuth0Env tnt
          res <- runClientM (signup pay) env
          isRight res `shouldBe` True

      describe "GetToken" $
        it "Resource Owner Password" $ do
          let pay = GetTokenResourceOwner Password cid (Just cst) (Just aud) usr pwd Nothing Nothing
          env <- mkAuth0Env tnt
          res <- runClientM (getTokenResourceOwner pay) env
          isRight res `shouldBe` True
          case res of
            Left _  -> return ()
            Right v -> tokenType v `shouldBe` "Bearer"

    -- Note: Make sure to assign the proper scopes to your machine to machine application
    describe "Management" $
      describe "Users" $
        it "Get Users" $ do
          -- authenticate
          let bdy = GetTokenClientCreds ClientCredentials cid cst aud
          env <- mkAuth0Env tnt
          res <- runClientM (getTokenClientCreds bdy) env
          isRight res `shouldBe` True

          -- use access token
          case res of
            Left _  -> return ()
            Right v -> do
              let f = usersGet ("Bearer " <> accessToken v)
                        Nothing Nothing Nothing Nothing Nothing
                        Nothing Nothing Nothing Nothing
              res' :: Either ServantError [UserResponse Value Value] <- runClientM f env
              isRight res' `shouldBe` True
