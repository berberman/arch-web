{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}

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

-- | SPDX License Identifiers included in <https://archlinux.org/packages/core/any/licenses/>,
data License
  = AGPL_3_0_only
  | AGPL_3_0_or_later
  | Apache_2_0
  | Artistic_1_0_Perl
  | Artistic_2_0
  | BSL_1_0
  | CC_BY_1_0
  | CC_BY_2_0
  | CC_BY_2_5
  | CC_BY_3_0_AT
  | CC_BY_3_0_US
  | CC_BY_3_0
  | CC_BY_4_0
  | CC_BY_NC_1_0
  | CC_BY_NC_2_0
  | CC_BY_NC_2_5
  | CC_BY_NC_3_0
  | CC_BY_NC_4_0
  | CC_BY_NC_ND_1_0
  | CC_BY_NC_ND_2_0
  | CC_BY_NC_ND_2_5
  | CC_BY_NC_ND_3_0_IGO
  | CC_BY_NC_ND_3_0
  | CC_BY_NC_ND_4_0
  | CC_BY_NC_SA_1_0
  | CC_BY_NC_SA_2_0
  | CC_BY_NC_SA_2_5
  | CC_BY_NC_SA_3_0
  | CC_BY_NC_SA_4_0
  | CC_BY_ND_1_0
  | CC_BY_ND_2_0
  | CC_BY_ND_2_5
  | CC_BY_ND_3_0
  | CC_BY_ND_4_0
  | CC_BY_SA_1_0
  | CC_BY_SA_2_0_UK
  | CC_BY_SA_2_1_JP
  | CC_BY_SA_2_5
  | CC_BY_SA_3_0_AT
  | CC_BY_SA_3_0
  | CC_BY_SA_4_0
  | CC_PDDC
  | CC0_1_0
  | CDDL_1_0
  | CDDL_1_1
  | CPL_1_0
  | EPL_1_0
  | EPL_2_0
  | FSFAP
  | GFDL_1_1_invariants_only
  | GFDL_1_1_invariants_or_later
  | GFDL_1_1_no_invariants_only
  | GFDL_1_1_no_invariants_or_later
  | GFDL_1_1_only
  | GFDL_1_1_or_later
  | GFDL_1_2_invariants_only
  | GFDL_1_2_invariants_or_later
  | GFDL_1_2_no_invariants_only
  | GFDL_1_2_no_invariants_or_later
  | GFDL_1_2_only
  | GFDL_1_2_or_later
  | GFDL_1_3_invariants_only
  | GFDL_1_3_invariants_or_later
  | GFDL_1_3_no_invariants_only
  | GFDL_1_3_no_invariants_or_later
  | GFDL_1_3_only
  | GFDL_1_3_or_later
  | GPL_1_0_only
  | GPL_1_0_or_later
  | GPL_2_0_only
  | GPL_2_0_or_later
  | GPL_3_0_only
  | GPL_3_0_or_later
  | LGPL_2_0_only
  | LGPL_2_0_or_later
  | LGPL_2_1_only
  | LGPL_2_1_or_later
  | LGPL_3_0_only
  | LGPL_3_0_or_later
  | LGPLLR
  | LPPL_1_0
  | LPPL_1_1
  | LPPL_1_2
  | LPPL_1_3a
  | LPPL_1_3c
  | MPL_1_0
  | MPL_1_1
  | MPL_2_0
  | PHP_3_0
  | PHP_3_01
  | PSF_2_0
  | Ruby
  | Unlicense
  | W3C
  | WTFPL
  | ZPL_1_1
  | ZPL_2_0
  | ZPL_2_1
  | Custom Text
  deriving stock (Show, Eq, Ord, Generic)

licenseId :: License -> Text
licenseId AGPL_3_0_only = "AGPL-3.0-only"
licenseId AGPL_3_0_or_later = "AGPL-3.0-or-later"
licenseId Apache_2_0 = "Apache-2.0"
licenseId Artistic_1_0_Perl = "Artistic-1.0-Perl"
licenseId Artistic_2_0 = "Artistic-2.0"
licenseId BSL_1_0 = "BSL-1.0"
licenseId CC_BY_1_0 = "CC-BY-1.0"
licenseId CC_BY_2_0 = "CC-BY-2.0"
licenseId CC_BY_2_5 = "CC-BY-2.5"
licenseId CC_BY_3_0_AT = "CC-BY-3.0-AT"
licenseId CC_BY_3_0_US = "CC-BY-3.0-US"
licenseId CC_BY_3_0 = "CC-BY-3.0"
licenseId CC_BY_4_0 = "CC-BY-4.0"
licenseId CC_BY_NC_1_0 = "CC-BY-NC-1.0"
licenseId CC_BY_NC_2_0 = "CC-BY-NC-2.0"
licenseId CC_BY_NC_2_5 = "CC-BY-NC-2.5"
licenseId CC_BY_NC_3_0 = "CC-BY-NC-3.0"
licenseId CC_BY_NC_4_0 = "CC-BY-NC-4.0"
licenseId CC_BY_NC_ND_1_0 = "CC-BY-NC-ND-1.0"
licenseId CC_BY_NC_ND_2_0 = "CC-BY-NC-ND-2.0"
licenseId CC_BY_NC_ND_2_5 = "CC-BY-NC-ND-2.5"
licenseId CC_BY_NC_ND_3_0_IGO = "CC-BY-NC-ND-3.0-IGO"
licenseId CC_BY_NC_ND_3_0 = "CC-BY-NC-ND-3.0"
licenseId CC_BY_NC_ND_4_0 = "CC-BY-NC-ND-4.0"
licenseId CC_BY_NC_SA_1_0 = "CC-BY-NC-SA-1.0"
licenseId CC_BY_NC_SA_2_0 = "CC-BY-NC-SA-2.0"
licenseId CC_BY_NC_SA_2_5 = "CC-BY-NC-SA-2.5"
licenseId CC_BY_NC_SA_3_0 = "CC-BY-NC-SA-3.0"
licenseId CC_BY_NC_SA_4_0 = "CC-BY-NC-SA-4.0"
licenseId CC_BY_ND_1_0 = "CC-BY-ND-1.0"
licenseId CC_BY_ND_2_0 = "CC-BY-ND-2.0"
licenseId CC_BY_ND_2_5 = "CC-BY-ND-2.5"
licenseId CC_BY_ND_3_0 = "CC-BY-ND-3.0"
licenseId CC_BY_ND_4_0 = "CC-BY-ND-4.0"
licenseId CC_BY_SA_1_0 = "CC-BY-SA-1.0"
licenseId CC_BY_SA_2_0_UK = "CC-BY-SA-2.0-UK"
licenseId CC_BY_SA_2_1_JP = "CC-BY-SA-2.1-JP"
licenseId CC_BY_SA_2_5 = "CC-BY-SA-2.5"
licenseId CC_BY_SA_3_0_AT = "CC-BY-SA-3.0-AT"
licenseId CC_BY_SA_3_0 = "CC-BY-SA-3.0"
licenseId CC_BY_SA_4_0 = "CC-BY-SA-4.0"
licenseId CC_PDDC = "CC-PDDC"
licenseId CC0_1_0 = "CC0-1.0"
licenseId CDDL_1_0 = "CDDL-1.0"
licenseId CDDL_1_1 = "CDDL-1.1"
licenseId CPL_1_0 = "CPL-1.0"
licenseId EPL_1_0 = "EPL-1.0"
licenseId EPL_2_0 = "EPL-2.0"
licenseId FSFAP = "FSFAP"
licenseId GFDL_1_1_invariants_only = "GFDL-1.1-invariants-only"
licenseId GFDL_1_1_invariants_or_later = "GFDL-1.1-invariants-or-later"
licenseId GFDL_1_1_no_invariants_only = "GFDL-1.1-no-invariants-only"
licenseId GFDL_1_1_no_invariants_or_later = "GFDL-1.1-no-invariants-or-later"
licenseId GFDL_1_1_only = "GFDL-1.1-only"
licenseId GFDL_1_1_or_later = "GFDL-1.1-or-later"
licenseId GFDL_1_2_invariants_only = "GFDL-1.2-invariants-only"
licenseId GFDL_1_2_invariants_or_later = "GFDL-1.2-invariants-or-later"
licenseId GFDL_1_2_no_invariants_only = "GFDL-1.2-no-invariants-only"
licenseId GFDL_1_2_no_invariants_or_later = "GFDL-1.2-no-invariants-or-later"
licenseId GFDL_1_2_only = "GFDL-1.2-only"
licenseId GFDL_1_2_or_later = "GFDL-1.2-or-later"
licenseId GFDL_1_3_invariants_only = "GFDL-1.3-invariants-only"
licenseId GFDL_1_3_invariants_or_later = "GFDL-1.3-invariants-or-later"
licenseId GFDL_1_3_no_invariants_only = "GFDL-1.3-no-invariants-only"
licenseId GFDL_1_3_no_invariants_or_later = "GFDL-1.3-no-invariants-or-later"
licenseId GFDL_1_3_only = "GFDL-1.3-only"
licenseId GFDL_1_3_or_later = "GFDL-1.3-or-later"
licenseId GPL_1_0_only = "GPL-1.0-only"
licenseId GPL_1_0_or_later = "GPL-1.0-or-later"
licenseId GPL_2_0_only = "GPL-2.0-only"
licenseId GPL_2_0_or_later = "GPL-2.0-or-later"
licenseId GPL_3_0_only = "GPL-3.0-only"
licenseId GPL_3_0_or_later = "GPL-3.0-or-later"
licenseId LGPL_2_0_only = "LGPL-2.0-only"
licenseId LGPL_2_0_or_later = "LGPL-2.0-or-later"
licenseId LGPL_2_1_only = "LGPL-2.1-only"
licenseId LGPL_2_1_or_later = "LGPL-2.1-or-later"
licenseId LGPL_3_0_only = "LGPL-3.0-only"
licenseId LGPL_3_0_or_later = "LGPL-3.0-or-later"
licenseId LGPLLR = "LGPLLR"
licenseId LPPL_1_0 = "LPPL-1.0"
licenseId LPPL_1_1 = "LPPL-1.1"
licenseId LPPL_1_2 = "LPPL-1.2"
licenseId LPPL_1_3a = "LPPL-1.3a"
licenseId LPPL_1_3c = "LPPL-1.3c"
licenseId MPL_1_0 = "MPL-1.0"
licenseId MPL_1_1 = "MPL-1.1"
licenseId MPL_2_0 = "MPL-2.0"
licenseId PHP_3_0 = "PHP-3.0"
licenseId PHP_3_01 = "PHP-3.01"
licenseId PSF_2_0 = "PSF-2.0"
licenseId Ruby = "Ruby"
licenseId Unlicense = "Unlicense"
licenseId W3C = "W3C"
licenseId WTFPL = "WTFPL"
licenseId ZPL_1_1 = "ZPL-1.1"
licenseId ZPL_2_0 = "ZPL-2.0"
licenseId ZPL_2_1 = "ZPL-2.1"
licenseId (Custom x) = "custom:" <> x

parseLicense :: Text -> License
parseLicense "AGPL-3.0-only" = AGPL_3_0_only
parseLicense "AGPL-3.0-or-later" = AGPL_3_0_or_later
parseLicense "Apache-2.0" = Apache_2_0
parseLicense "Artistic-1.0-Perl" = Artistic_1_0_Perl
parseLicense "Artistic-2.0" = Artistic_2_0
parseLicense "BSL-1.0" = BSL_1_0
parseLicense "CC-BY-1.0" = CC_BY_1_0
parseLicense "CC-BY-2.0" = CC_BY_2_0
parseLicense "CC-BY-2.5" = CC_BY_2_5
parseLicense "CC-BY-3.0-AT" = CC_BY_3_0_AT
parseLicense "CC-BY-3.0-US" = CC_BY_3_0_US
parseLicense "CC-BY-3.0" = CC_BY_3_0
parseLicense "CC-BY-4.0" = CC_BY_4_0
parseLicense "CC-BY-NC-1.0" = CC_BY_NC_1_0
parseLicense "CC-BY-NC-2.0" = CC_BY_NC_2_0
parseLicense "CC-BY-NC-2.5" = CC_BY_NC_2_5
parseLicense "CC-BY-NC-3.0" = CC_BY_NC_3_0
parseLicense "CC-BY-NC-4.0" = CC_BY_NC_4_0
parseLicense "CC-BY-NC-ND-1.0" = CC_BY_NC_ND_1_0
parseLicense "CC-BY-NC-ND-2.0" = CC_BY_NC_ND_2_0
parseLicense "CC-BY-NC-ND-2.5" = CC_BY_NC_ND_2_5
parseLicense "CC-BY-NC-ND-3.0-IGO" = CC_BY_NC_ND_3_0_IGO
parseLicense "CC-BY-NC-ND-3.0" = CC_BY_NC_ND_3_0
parseLicense "CC-BY-NC-ND-4.0" = CC_BY_NC_ND_4_0
parseLicense "CC-BY-NC-SA-1.0" = CC_BY_NC_SA_1_0
parseLicense "CC-BY-NC-SA-2.0" = CC_BY_NC_SA_2_0
parseLicense "CC-BY-NC-SA-2.5" = CC_BY_NC_SA_2_5
parseLicense "CC-BY-NC-SA-3.0" = CC_BY_NC_SA_3_0
parseLicense "CC-BY-NC-SA-4.0" = CC_BY_NC_SA_4_0
parseLicense "CC-BY-ND-1.0" = CC_BY_ND_1_0
parseLicense "CC-BY-ND-2.0" = CC_BY_ND_2_0
parseLicense "CC-BY-ND-2.5" = CC_BY_ND_2_5
parseLicense "CC-BY-ND-3.0" = CC_BY_ND_3_0
parseLicense "CC-BY-ND-4.0" = CC_BY_ND_4_0
parseLicense "CC-BY-SA-1.0" = CC_BY_SA_1_0
parseLicense "CC-BY-SA-2.0-UK" = CC_BY_SA_2_0_UK
parseLicense "CC-BY-SA-2.1-JP" = CC_BY_SA_2_1_JP
parseLicense "CC-BY-SA-2.5" = CC_BY_SA_2_5
parseLicense "CC-BY-SA-3.0-AT" = CC_BY_SA_3_0_AT
parseLicense "CC-BY-SA-3.0" = CC_BY_SA_3_0
parseLicense "CC-BY-SA-4.0" = CC_BY_SA_4_0
parseLicense "CC-PDDC" = CC_PDDC
parseLicense "CC0-1.0" = CC0_1_0
parseLicense "CDDL-1.0" = CDDL_1_0
parseLicense "CDDL-1.1" = CDDL_1_1
parseLicense "CPL-1.0" = CPL_1_0
parseLicense "EPL-1.0" = EPL_1_0
parseLicense "EPL-2.0" = EPL_2_0
parseLicense "FSFAP" = FSFAP
parseLicense "GFDL-1.1-invariants-only" = GFDL_1_1_invariants_only
parseLicense "GFDL-1.1-invariants-or-later" = GFDL_1_1_invariants_or_later
parseLicense "GFDL-1.1-no-invariants-only" = GFDL_1_1_no_invariants_only
parseLicense "GFDL-1.1-no-invariants-or-later" = GFDL_1_1_no_invariants_or_later
parseLicense "GFDL-1.1-only" = GFDL_1_1_only
parseLicense "GFDL-1.1-or-later" = GFDL_1_1_or_later
parseLicense "GFDL-1.2-invariants-only" = GFDL_1_2_invariants_only
parseLicense "GFDL-1.2-invariants-or-later" = GFDL_1_2_invariants_or_later
parseLicense "GFDL-1.2-no-invariants-only" = GFDL_1_2_no_invariants_only
parseLicense "GFDL-1.2-no-invariants-or-later" = GFDL_1_2_no_invariants_or_later
parseLicense "GFDL-1.2-only" = GFDL_1_2_only
parseLicense "GFDL-1.2-or-later" = GFDL_1_2_or_later
parseLicense "GFDL-1.3-invariants-only" = GFDL_1_3_invariants_only
parseLicense "GFDL-1.3-invariants-or-later" = GFDL_1_3_invariants_or_later
parseLicense "GFDL-1.3-no-invariants-only" = GFDL_1_3_no_invariants_only
parseLicense "GFDL-1.3-no-invariants-or-later" = GFDL_1_3_no_invariants_or_later
parseLicense "GFDL-1.3-only" = GFDL_1_3_only
parseLicense "GFDL-1.3-or-later" = GFDL_1_3_or_later
parseLicense "GPL-1.0-only" = GPL_1_0_only
parseLicense "GPL-1.0-or-later" = GPL_1_0_or_later
parseLicense "GPL-2.0-only" = GPL_2_0_only
parseLicense "GPL-2.0-or-later" = GPL_2_0_or_later
parseLicense "GPL-3.0-only" = GPL_3_0_only
parseLicense "GPL-3.0-or-later" = GPL_3_0_or_later
parseLicense "LGPL-2.0-only" = LGPL_2_0_only
parseLicense "LGPL-2.0-or-later" = LGPL_2_0_or_later
parseLicense "LGPL-2.1-only" = LGPL_2_1_only
parseLicense "LGPL-2.1-or-later" = LGPL_2_1_or_later
parseLicense "LGPL-3.0-only" = LGPL_3_0_only
parseLicense "LGPL-3.0-or-later" = LGPL_3_0_or_later
parseLicense "LGPLLR" = LGPLLR
parseLicense "LPPL-1.0" = LPPL_1_0
parseLicense "LPPL-1.1" = LPPL_1_1
parseLicense "LPPL-1.2" = LPPL_1_2
parseLicense "LPPL-1.3a" = LPPL_1_3a
parseLicense "LPPL-1.3c" = LPPL_1_3c
parseLicense "MPL-1.0" = MPL_1_0
parseLicense "MPL-1.1" = MPL_1_1
parseLicense "MPL-2.0" = MPL_2_0
parseLicense "PHP-3.0" = PHP_3_0
parseLicense "PHP-3.01" = PHP_3_01
parseLicense "PSF-2.0" = PSF_2_0
parseLicense "Ruby" = Ruby
parseLicense "Unlicense" = Unlicense
parseLicense "W3C" = W3C
parseLicense "WTFPL" = WTFPL
parseLicense "ZPL-1.1" = ZPL_1_1
parseLicense "ZPL-2.0" = ZPL_2_0
parseLicense "ZPL-2.1" = ZPL_2_1
parseLicense x = Custom x

instance ToJSON License where
  toJSON x = String $ licenseId x

instance FromJSON License where
  parseJSON = withText "License" $ \txt -> pure (parseLicense txt)

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
