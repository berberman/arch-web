{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module defines types and their serializations used in API.
--
-- Fields' names are prefixed with @_@ for lenses generation,
-- consider using "Web.ArchLinux.Types.Lens" to access data types smoothly.
module Web.ArchLinux.Types
  ( -- * Arch Linux official
    Repo (..),
    Arch (..),
    License (..),
    licenseId,
    parseLicense,
    PackageInformation (..),
    PackageFiles (..),
    Flagged (..),
    ArchLinuxResponse (..),

    -- * AUR
    AurSearch (..),
    AurInfo (..),
    AurResponseType (..),
    AurResponse (..),
  )
where

import Data.Aeson
import Data.Char (toUpper)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Deriving.Aeson
import Servant.API (ToHttpApiData (..))
import Web.ArchLinux.Internal.KnownSPDXLoader

type ArchLinuxJSON = CustomJSON '[FieldLabelModifier (StripPrefix "_", CamelToSnake)]

unString :: Value -> Text
unString (String x) = x
unString _ = undefined

toQueryParamViaJSON :: (ToJSON a) => a -> Text
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

-- | Official repositories.
data Repo
  = Core
  | CoreTesting
  | Extra
  | ExtraTesting
  | Multilib
  | MultilibTesting
  | GnomeUnstable
  | KDEUnstable
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier CamelToKebab] Repo

instance ToHttpApiData Repo where
  toQueryParam (toQueryParamViaJSON -> x) = case T.breakOn "-" x of
    (T.uncons -> Just (h, s), "") -> T.cons (toUpper h) s
    (T.uncons -> Just (h1, s1), T.uncons -> Just ('-', T.uncons -> Just (h2, s2))) ->
      T.cons (toUpper h1) s1 <> "-" <> T.cons (toUpper h2) s2
    _ -> undefined

-- | Official architectures.
data Arch
  = Any
  | I686
  | X86_64
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier CamelToSnake] Arch

instance ToHttpApiData Arch where
  toQueryParam = toQueryParamViaJSON

$(loadKnownSPDX)

licenseId :: License -> String
parseLicense :: String -> License

instance ToJSON License where
  toJSON x = String . T.pack $ licenseId x

instance FromJSON License where
  parseJSON = withText "License" $ \(T.unpack -> txt) -> pure (parseLicense txt)

-- | Package details returned by 'Web.ArchLinux.API.getPackageDetails'.
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

-- | Package files list returned by 'Web.ArchLinux.API.getPackageFiles'
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

-- | Flagged package means out-of-date.
data Flagged = Flagged | NotFlagged
  deriving stock (Show, Eq, Ord, Enum, Generic)

instance ToHttpApiData Flagged where
  toQueryParam Flagged = "Flagged"
  toQueryParam NotFlagged = "Not+Flagged"

-- | Response data type of 'Web.ArchLinux.API.searchPackage'.
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

-- | Search results returned by 'Web.ArchLinux.API.searchAur'.
--
-- Some of fields are renamed in this record type, for sharing
-- overloaded lenses between data type returned by Arch Linux official API.
data AurSearch = AurSearch
  { _id :: Int,
    _name :: Text,
    _packageBaseID :: Int,
    _packageBase :: Text,
    _version :: Text,
    _description :: Maybe Text,
    -- | URL
    _url :: Maybe Text,
    _numVotes :: Int,
    _popularity :: Double,
    -- | UTCTime
    _outOfDate :: Maybe Int,
    _maintainer :: Maybe Text,
    -- | UTCTime
    _firstSubmitted :: Int,
    -- | UTCTime
    _lastModified :: Int,
    _urlPath :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via AurJSON AurSearch

-- | Package details returned by 'Web.ArchLinux.API.getAurInfo'.
--
-- This data type extends 'AurSearch' informally,
-- so it includes '_search' as a member.
data AurInfo = AurInfo
  { -- | ID, Name, PackageBaseID, ...
    _search :: AurSearch,
    -- | Depends
    _depends :: [Text],
    -- | MakeDepends
    _makedepends :: [Text],
    -- | OptDepends
    _optdepends :: [Text],
    -- | CheckDepends
    _checkdepends :: [Text],
    _conflicts :: [Text],
    _provides :: [Text],
    _replaces :: [Text],
    _groups :: [Text],
    _licenses :: [License],
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

-- | Return types of AUR API.
data AurResponseType = Search | Multiinfo | Error
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier CamelToSnake] AurResponseType

-- | Response data type of AUR API.
data AurResponse a = AurResponse
  { _version :: Int,
    -- | type
    _aurType :: AurResponseType,
    -- | resultcount
    _resultCount :: Int,
    _results :: a,
    -- | Available when 'aurType' equals to 'AurResponseType.Error'.
    _error :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)

instance (FromJSON a) => FromJSON (AurResponse a) where
  parseJSON = withObject "AurResponse" $ \o -> do
    _version <- o .: "version"
    _aurType <- o .: "type"
    _resultCount <- o .: "resultcount"
    _results <- o .: "results"
    _error <- o .:? "error"
    pure AurResponse {..}

instance (ToJSON a) => ToJSON (AurResponse a) where
  toJSON AurResponse {..} =
    object $
      [ "version" .= _version,
        "type" .= _aurType,
        "resultcount" .= _resultCount,
        "results" .= _results
      ]
        <> case _error of
          Just err -> ["error" .= err]
          _ -> []
