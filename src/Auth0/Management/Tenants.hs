{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Auth0.Management.Tenants where

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Text
import GHC.Generics
--------------------------------------------------------------------------------
import Auth0.Request
import Auth0.Types
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- GET /api/v2/tenants/settings

-- Request

data TenantSettings
  = TenantSettings
  { fields        :: Maybe Text
  , includeFields :: Maybe Text
  } deriving (Show)

instance ToRequest TenantSettings where
  toRequest (TenantSettings a b) =
    [ toField "fields" a
    , toField "include_fields" b
    ]

-- Response

data ChangePassword
  = ChangePassword
  { enabled :: Maybe Bool
  , html    :: Maybe Text
  } deriving (Generic, Show)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''ChangePassword

data GuardianMfaPage
  = GuardianMfaPage
  { enabled :: Maybe Bool
  , html    :: Maybe Text
  } deriving (Generic, Show)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''GuardianMfaPage

data ErrorPage
  = ErrorPage
  { html        :: Maybe Text
  , showLogLink :: Maybe Bool
  , url         :: Maybe Text
  } deriving (Generic, Show)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''ErrorPage

data Flags
  = Flags
  { changePwdFlowV1         :: Maybe Bool
  , enableApisSection       :: Maybe Bool
  , disableImpersonation    :: Maybe Bool
  , enableClientConnections :: Maybe Bool
  , enablePipeline2         :: Maybe Bool
  } deriving (Generic, Show)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''Flags

data TenantSettingResponse
  = TenantSettingResponse
  { changePassword    :: Maybe ChangePassword
  , guardianMfaPage   :: Maybe GuardianMfaPage
  , defaultAudience   :: Maybe Text
  , defaultDirectory  :: Maybe Text
  , errorPage         :: Maybe ErrorPage
  , flags             :: Maybe Flags
  , friendlyName      :: Maybe Text
  , pictureUrl        :: Maybe Text
  , supportEmail      :: Maybe Text
  , supportUrl        :: Maybe Text
  , allowedLogoutUrls :: Maybe [Text]
  , sessionLifetime   :: Maybe Double
  } deriving (Generic, Show)

deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''TenantSettingResponse

runGetTenantSettings
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> TenantSettings -> m (Auth0Response TenantSettingResponse)
runGetTenantSettings (TokenAuth tenant accessToken) o =
  let api = API Get "/api/v2/tenants/settings"
  in execRequest tenant api (Just o) (Nothing :: Maybe ()) (Just [mkAuthHeader accessToken])

--------------------------------------------------------------------------------
-- PATCH /api/v2/tenants/settings

type TenantSettingsUpdate = TenantSettingResponse

runUpdateTenantSettings
  :: (MonadIO m, MonadThrow m)
  => TokenAuth -> TenantSettingsUpdate -> m (Auth0Response TenantSettingResponse)
runUpdateTenantSettings (TokenAuth tenant accessToken) o =
  let api = API Update "/api/v2/tenants/settings"
  in execRequest tenant api (Nothing :: Maybe ()) (Just o) (Just [mkAuthHeader accessToken])
