{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.ArchLinux.Types where

import Data.Aeson
import Data.Char (toUpper)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Deriving.Aeson
import Servant.API (ToHttpApiData (..))

type ArchLinuxJSON = CustomJSON '[FieldLabelModifier (StripPrefix "_", CamelToSnake)]

unString :: Value -> Text
unString (String x) = x
unString _ = undefined

toQueryParamViaJSON :: ToJSON a => a -> Text
toQueryParamViaJSON = unString . fromJust . decode . encode

data AurModifier

instance StringModifier AurModifier where
  getStringModifier (T.pack -> t) = T.unpack $ eat "Url" "URL" $ eat "Id" "ID" upper
    where
      upper =
        case T.uncons t of
          Just (h, s) -> T.cons (toUpper h) s
          _ -> error "impossible"
      eat prefix to input = case T.stripPrefix prefix input of
        Just x -> to <> x
        _ -> input

type AurJSON = CustomJSON '[FieldLabelModifier (StripPrefix "_", AurModifier)]

-----------------------------------------------------------------------------
data Repo
  = Core
  | Extra
  | Testing
  | Multilib
  | MultilibTesting
  | Community
  | CommunityTesting
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier CamelToKebab] Repo

instance ToHttpApiData Repo where
  toQueryParam (toQueryParamViaJSON -> x) = case T.breakOn "-" x of
    (T.uncons -> Just (h, s), "") -> T.cons (toUpper h) s
    (T.uncons -> Just (h1, s1), T.uncons -> Just ('-', T.uncons -> Just (h2, s2))) ->
      T.cons (toUpper h1) s1 <> "-" <> T.cons (toUpper h2) s2
    _ -> undefined

data Arch
  = Any
  | I686
  | X86_64
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier CamelToSnake] Arch

instance ToHttpApiData Arch where
  toQueryParam = toQueryParamViaJSON

data License
  = AGPL3
  | Apache
  | Artistic2_0
  | CDDL
  | CPL
  | EPL
  | FDL1_2
  | FDL1_3
  | GPL2
  | GPL3
  | LGPL2_1
  | LGPL3
  | LPPL
  | MPL
  | MPL2
  | PHP
  | PSF
  | PerlArtistic
  | RUBY
  | Unlicense
  | W3C
  | ZPL
  | Custom Text
  deriving stock (Show, Eq, Ord, Generic)

licenseJSONOption :: Options
licenseJSONOption =
  defaultOptions
    { fieldLabelModifier = T.unpack . T.replace "_" "." . T.pack,
      sumEncoding = UntaggedValue
    }

instance ToJSON License where
  toJSON (Custom x) = String $ "custom: " <> x
  toJSON x =
    genericToJSON licenseJSONOption x

instance FromJSON License where
  parseJSON = withText "License" $ \txt -> do
    case T.stripPrefix "custom:" txt of
      Just c -> pure $ Custom c
      _ -> genericParseJSON licenseJSONOption (String $ T.stripStart txt)

data PackageInformation = PackageInformation
  { _pkgname :: Text,
    _pkgbase :: Text,
    _repo :: Repo,
    _arch :: Arch,
    _pkgver :: Text,
    _pkgrel :: Text,
    _epoch :: Int,
    _pkgdesc :: Text,
    _url :: Text,
    _filename :: Text,
    _compressedSize :: Int,
    _installedSize :: Int,
    _buildDate :: UTCTime,
    _lastUpdate :: UTCTime,
    _flageDate :: Maybe UTCTime,
    _maintainers :: [Text],
    _packager :: Text,
    _groups :: [Text],
    _licenses :: [License],
    _conflicts :: [Text],
    _provides :: [Text],
    _replaces :: [Text],
    _depends :: [Text],
    _optdepends :: [Text],
    _makedepends :: [Text],
    _checkdepends :: [Text]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via ArchLinuxJSON PackageInformation

data PackageFiles = PackageFiles
  { _pkgname :: Text,
    _repo :: Repo,
    _arch :: Arch,
    _pkgLastUpdate :: UTCTime,
    _filesLastUpdate :: UTCTime,
    _filesCount :: Int,
    _dirCount :: Int,
    _files :: [FilePath]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via ArchLinuxJSON PackageFiles

data Flagged = Flagged | NotFlagged
  deriving stock (Show, Eq, Ord, Enum, Generic)

instance ToHttpApiData Flagged where
  toQueryParam Flagged = "Flagged"
  toQueryParam NotFlagged = "Not+Flagged"

data ArchLinuxResponse a = ArchLinuxResponse
  { _version :: Int,
    _limit :: Int,
    _valid :: Bool,
    _results :: [a]
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)

deriving via ArchLinuxJSON (ArchLinuxResponse a) instance (FromJSON a) => FromJSON (ArchLinuxResponse a)

deriving via ArchLinuxJSON (ArchLinuxResponse a) instance (ToJSON a) => ToJSON (ArchLinuxResponse a)

-----------------------------------------------------------------------------

data AurSearch = AurSearch
  { _id :: Int,
    _name :: Text,
    _packageBaseID :: Int,
    _packageBase :: Text,
    _version :: Text,
    _description :: Text,
    -- | @URL@
    _url :: Text,
    _numVotes :: Int,
    _popularity :: Double,
    -- | UTCTime
    _outOfDate :: Maybe Int,
    _maintainer :: Text,
    -- | UTCTime
    _firstSubmitted :: Int,
    -- | UTCTime
    _lastModified :: Int,
    _urlPath :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via AurJSON AurSearch

data AurInfo = AurInfo
  { -- | @ID, Name, PackageBaseID, ...@
    _search :: AurSearch,
    -- | @Depends@
    _depends :: [Text],
    -- | @MakeDepends@
    _makedepends :: [Text],
    -- | @OptDepends@
    _optdepends :: [Text],
    -- | @CheckDepends@
    _checkdepends :: [Text],
    _conflicts :: [Text],
    _provides :: [Text],
    _replaces :: [Text],
    _groups :: [Text],
    _licenses :: [Text],
    _keywords :: [Text]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON AurInfo where
  parseJSON = withObject "AurInfo" $ \o -> do
    _search <- parseJSON $ Object o
    _depends <- fromMaybe [] <$> o .:? "Depends"
    _makedepends <- fromMaybe [] <$> o .:? "MakeDepends"
    _optdepends <- fromMaybe [] <$> o .:? "OptDepends"
    _checkdepends <- fromMaybe [] <$> o .:? "CheckDepends"
    _conflicts <- fromMaybe [] <$> o .:? "Conflicts"
    _provides <- fromMaybe [] <$> o .:? "Provides"
    _replaces <- fromMaybe [] <$> o .:? "Replaces"
    _groups <- fromMaybe [] <$> o .:? "Groups"
    _licenses <- fromMaybe [] <$> o .:? "License"
    _keywords <- fromMaybe [] <$> o .:? "Keywords"
    pure AurInfo {..}

instance ToJSON AurInfo where
  toJSON AurInfo {..} =
    Object $
      unObject (toJSON _search)
        <> unObject
          ( object
              [ "Depends" .= _depends,
                "MakeDepends" .= _makedepends,
                "OptDepends" .= _optdepends,
                "CheckDepends" .= _checkdepends,
                "Conflicts" .= _conflicts,
                "Provides" .= _provides,
                "Replaces" .= _replaces,
                "Groups" .= _groups,
                "License" .= _licenses,
                "Keywords" .= _keywords
              ]
          )
    where
      unObject (Object o) = o
      unObject _ = error "impossible"

data AurResponseType = Search | Multiinfo | Error
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier CamelToSnake] AurResponseType

data AurResponse a = AurResponse
  { _version :: Int,
    _type :: AurResponseType,
    _resultcount :: Int,
    _results :: a,
    _error :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)

deriving via ArchLinuxJSON (AurResponse a) instance (FromJSON a) => FromJSON (AurResponse a)

deriving via ArchLinuxJSON (AurResponse a) instance (ToJSON a) => ToJSON (AurResponse a)
