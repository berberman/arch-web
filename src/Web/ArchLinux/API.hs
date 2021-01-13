{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.ArchLinux.API where

import Data.Aeson (FromJSON, Result (..), Value, fromJSON)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (http11)
import Servant.Client
import Servant.Client.Core (throwClientError)
import Web.ArchLinux.Types
import Web.ArchLinux.Types.API

data APIType = ArchLinux | Aur

newtype APIClient (k :: APIType) a = APIClient {unWrapClientM :: ClientM a}

class HasBaseUrl (k :: APIType) where
  getBaseUrl :: BaseUrl

instance HasBaseUrl 'ArchLinux where
  getBaseUrl = BaseUrl Https "www.archlinux.org" 443 ""

instance HasBaseUrl 'Aur where
  getBaseUrl = BaseUrl Https "aur.archlinux.org" 443 ""

-----------------------------------------------------------------------------

data SearchOptions = SearchOptions
  { nameOrDescription :: Maybe Text,
    exactName :: Maybe Text,
    description :: Maybe Text,
    repositories :: [Repo],
    architectures :: [Arch],
    targetMaintianer :: Maybe Text,
    targetPackager :: Maybe Text,
    isFlagged :: Maybe Flagged
  }
  deriving stock (Generic, Eq, Ord, Show)

emptySearchOptions :: SearchOptions
emptySearchOptions = SearchOptions Nothing Nothing Nothing [] [] Nothing Nothing Nothing

getPackageDetails :: Repo -> Arch -> Text -> APIClient 'ArchLinux PackageInformation
getPackageDetails r a p = APIClient $ client (Proxy @GetPackageDetails) r a p

getPackageFiles :: Repo -> Arch -> Text -> APIClient 'ArchLinux PackageFiles
getPackageFiles r a p = APIClient $ client (Proxy @GetPackageFiles) r a p

searchPackage :: SearchOptions -> APIClient 'ArchLinux (ArchLinuxResponse PackageInformation)
searchPackage SearchOptions {..} =
  let f = client (Proxy @SearchPackage)
   in APIClient $
        f
          nameOrDescription
          exactName
          description
          repositories
          architectures
          targetMaintianer
          targetPackager
          isFlagged

-----------------------------------------------------------------------------

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

-- This is evil and cheating!! Use this to report delayed json decode error
dummyResponse :: Response
dummyResponse = Response (toEnum 0) mempty http11 "dummy response for error display"

parseResult :: (FromJSON a) => Value -> ClientM a
parseResult v = case fromJSON v of
  Success x -> pure x
  Data.Aeson.Error err -> throwClientError $ DecodeFailure (T.pack err) dummyResponse

searchAur :: AurSearchType -> Text -> APIClient 'Aur (AurResponse AurSearch)
searchAur (searchTypeToValue -> f) arg = APIClient $ do
  result <- aurRPC "search" (Just f) [arg]
  parseResult result

getAurInfo :: [Text] -> APIClient 'Aur (AurResponse [AurInfo])
getAurInfo exactNames = APIClient $ do
  result <- aurRPC "info" Nothing exactNames
  parseResult result

-----------------------------------------------------------------------------

runClient :: forall s a. HasBaseUrl s => Manager -> APIClient s a -> IO (Either ClientError a)
runClient manager m = runClientM (unWrapClientM m) $ mkClientEnv manager $ getBaseUrl @s
