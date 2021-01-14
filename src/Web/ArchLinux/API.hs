{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- Arch Linux official repositories APIs and AUR APIs.
--
-- These two kinds of APIs are distinguished by 'APIClient'.
-- Functions over them return 'APIClient' parametrize by corresponding 'APIType'.
--
-- Overall, there are five APIs available,
-- refer to <https://wiki.archlinux.org/index.php/Official_repositories_web_interface> and <https://wiki.archlinux.org/index.php/Aurweb_RPC_interface>.
module Web.ArchLinux.API
  ( -- * API Client
    APIClient (..),
    APIType (..),
    HasBaseUrl (..),
    runAPIClient,
    runAPIClient',

    -- * Arch Linux official
    SearchOptions (..),
    emptySearchOptions,
    getPackageDetails,
    getPackageFiles,
    searchPackage,

    -- * AUR
    AurSearchType (..),
    searchAur,
    getAurInfo,
  )
where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, Result (..), Value, fromJSON)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (http11)
import Servant.Client
import Servant.Client.Core (RunClient, throwClientError)
import Web.ArchLinux.Types
import Web.ArchLinux.Types.API

-- | Two types of APIs.
data APIType = ArchLinux | Aur

-- | A wrapper of 'ClientM', with 'BaseUrl' reflected to type level phantom 'APIType'.
newtype APIClient (k :: APIType) a = APIClient {unWrapClientM :: ClientM a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      Generic,
      MonadReader ClientEnv,
      MonadError ClientError,
      MonadThrow,
      MonadCatch
    )
  deriving (RunClient) via ClientM

-- | Class to reify 'BaseUrl' from 'APIType'.
class HasBaseUrl (k :: APIType) where
  getBaseUrl :: BaseUrl

instance HasBaseUrl 'ArchLinux where
  getBaseUrl = BaseUrl Https "www.archlinux.org" 443 ""

instance HasBaseUrl 'Aur where
  getBaseUrl = BaseUrl Https "aur.archlinux.org" 443 ""

-- | Runs 'APIClient'.
--
-- It calls 'getBaseUrl', then creates 'ClientEnv', finally calls 'runClientM'.
runAPIClient :: forall s a. HasBaseUrl s => Manager -> APIClient s a -> IO (Either ClientError a)
runAPIClient manager m = runClientM (unWrapClientM m) $ mkClientEnv manager $ getBaseUrl @s

-- | Like 'runAPIClient', but creates a 'Manager'.
runAPIClient' :: HasBaseUrl s => APIClient s a -> IO (Either ClientError a)
runAPIClient' m = newTlsManager >>= flip runAPIClient m

-----------------------------------------------------------------------------

-- | Options available in searching packages in Arch Linux official repositories.
--
-- See 'searchPackage'.
data SearchOptions = SearchOptions
  { _nameOrDescription :: Maybe Text,
    _exactName :: Maybe Text,
    _targetDescription :: Maybe Text,
    _targetRepositories :: [Repo],
    _targetArchitectures :: [Arch],
    _targetMaintianer :: Maybe Text,
    _targetPackager :: Maybe Text,
    _isFlagged :: Maybe Flagged
  }
  deriving stock (Generic, Eq, Ord, Show)

-- | An empty options value for convenient.
--
-- For example,
-- 
-- > let options =
-- >       emptySearchOptions
-- >         & nameOrDescription ?~ "kea"
-- >         & targetRepositories .~ [Community, CommunityTesting]
-- > searchPackage options
-- 
-- searchs packages whose names or descriptions contain @kea@, from @Community@ or @Community-Testing@.
emptySearchOptions :: SearchOptions
emptySearchOptions = SearchOptions Nothing Nothing Nothing [] [] Nothing Nothing Nothing

-- | Gets details of an exact package.
getPackageDetails ::
  -- | official repository
  Repo ->
  -- | arch
  Arch ->
  -- | exact name
  Text ->
  APIClient 'ArchLinux PackageInformation
getPackageDetails r a p = APIClient $ client (Proxy @GetPackageDetails) r a p

-- | Gets files list of an exact package.
getPackageFiles ::
  -- | official repository
  Repo ->
  -- | arch
  Arch ->
  -- | exact name
  Text ->
  APIClient 'ArchLinux PackageFiles
getPackageFiles r a p = APIClient $ client (Proxy @GetPackageFiles) r a p

-- | Searches packages.
--
-- See 'SearchOptions' and 'emptySearchOptions'.
searchPackage :: SearchOptions -> APIClient 'ArchLinux (ArchLinuxResponse PackageInformation)
searchPackage SearchOptions {..} =
  let f = client (Proxy @SearchPackage)
   in APIClient $
        f
          _nameOrDescription
          _exactName
          _targetDescription
          _targetRepositories
          _targetArchitectures
          _targetMaintianer
          _targetPackager
          _isFlagged

-----------------------------------------------------------------------------

-- | Searches packages in AUR by what?
data AurSearchType
  = ByName
  | ByNameOrDesc
  | ByMaintainer
  | ByDepends
  | ByMakedepends
  | ByOptdepends
  | ByCheckdepends
  deriving stock (Generic, Eq, Ord, Enum, Show)

searchTypeToValue :: AurSearchType -> Text
searchTypeToValue = \case
  ByName -> "name"
  ByNameOrDesc -> "name-desc"
  ByMaintainer -> "maintainer"
  ByDepends -> "depends"
  ByMakedepends -> "makedepends"
  ByOptdepends -> "optdepends"
  ByCheckdepends -> "checkdepends"

aurRPC :: Text -> Maybe Text -> [Text] -> ClientM Value
aurRPC = client (Proxy @AurRPC) 5

-- This is evil and cheating!!
-- Use this to report delayed json decode error in 'HttpClientError'
dummyResponse :: Response
dummyResponse = Response (toEnum 0) mempty http11 "dummy response for error display"

parseResult :: (FromJSON a) => Value -> ClientM a
parseResult v = case fromJSON v of
  Success x -> pure x
  Data.Aeson.Error err -> throwClientError $ DecodeFailure (T.pack err) dummyResponse

-- | Searches packages in AUR.
searchAur ::
  -- | search type
  AurSearchType ->
  -- | search argument
  Text ->
  APIClient 'Aur (AurResponse [AurSearch])
searchAur (searchTypeToValue -> f) arg = APIClient $ do
  result <- aurRPC "search" (Just f) [arg]
  parseResult result

-- | Gets details of a set of packages in AUR.
getAurInfo ::
  -- | exact names
  [Text] ->
  APIClient 'Aur (AurResponse [AurInfo])
getAurInfo exactNames = APIClient $ do
  result <- aurRPC "info" Nothing exactNames
  parseResult result
