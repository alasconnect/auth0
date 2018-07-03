{-# LANGUAGE ScopedTypeVariables #-}

module Main where

--------------------------------------------------------------------------------
import Data.Aeson
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (pack)
import System.Environment (lookupEnv)
import System.Random
--------------------------------------------------------------------------------
import Test.Hspec
--------------------------------------------------------------------------------
import Auth0.Authentication.GetToken
import Auth0.Authentication.Signup
import Auth0.Management.Users
import Auth0.Types
--------------------------------------------------------------------------------

main :: IO ()
main = do
  tnt <- lookupEnv "AUTH0_TENANT"
  cid <- lookupEnv "AUTH0_CLIENT_ID"
  cst <- lookupEnv "AUTH0_CLIENT_SECRET"
  aud <- lookupEnv "AUTH0_AUDIENCE"
  con <- lookupEnv "AUTH0_CONNECTION"

  g <- getStdGen
  let usr = (pack . take 32) (randomRs ('a', 'z') g) <> "@example.com"
      pwd = "TestAuth0!"

  hspec $ do
    describe "Authentication.GetToken" $
      it "Client Credentials" $ do
        let ath = mkAuth ((BS.pack . fromJust) tnt)
            pay = GetTokenClientCreds
                    ClientCredentials
                    ((mkClientId . pack . fromJust) cid)
                    ((mkClientSecret . pack . fromJust) cst)
                    ((pack . fromJust) aud)
        res <- runGetToken ath pay
        resError res `shouldBe` Nothing
        isJust (resPayload res) `shouldBe` True
        case resPayload res of
          Nothing   -> return ()
          Just res' -> tokenType res' `shouldBe` "Bearer"

    describe "Authentication.Signup" $
      it "Signup" $ do
        let ath = mkAuth ((BS.pack . fromJust) tnt)
            pay = Signup
                    ((mkClientId . pack . fromJust) cid)
                    usr
                    pwd
                    ((pack . fromJust) con)
                    Nothing
        res <- runSignup ath pay
        resError res `shouldBe` Nothing
        isJust (resPayload res) `shouldBe` True

    describe "Authentication.GetToken" $
      it "Resource Owner Password" $ do
        let ath = mkAuth ((BS.pack . fromJust) tnt)
            pay = GetTokenResourceOwner
                    Password
                    ((mkClientId . pack . fromJust) cid)
                    ((Just . mkClientSecret . pack . fromJust) cst)
                    ((Just . pack . fromJust) aud)
                    usr
                    pwd
                    Nothing
                    Nothing
        res <- runGetToken ath pay
        resError res `shouldBe` Nothing
        isJust (resPayload res) `shouldBe` True
        case resPayload res of
          Nothing   -> return ()
          Just res' -> tokenType res' `shouldBe` "Bearer"

    describe "Management.Users" $
      it "Get Users" $ do
        let auth = mkAuth ((BS.pack . fromJust) tnt)
            pay = GetTokenClientCreds
                  ClientCredentials
                  (mkClientId . pack . fromJust $ cid)
                  (mkClientSecret . pack . fromJust $ cst)
                  (pack . fromJust $ aud)
        tknResponse <- runGetToken auth pay
        let tokenAuth = fromJust $ do
             tkn <- accessToken <$> resPayload tknResponse
             pure $ mkTokenAuth ((BS.pack . fromJust) tnt) tkn
        res :: Auth0Response [UserResponse Value Value] <- runGetUsers tokenAuth Nothing
        resError res `shouldBe` Nothing
        isJust (resPayload res) `shouldBe` True
