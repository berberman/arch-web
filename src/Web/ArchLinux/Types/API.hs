module Web.ArchLinux.Types.API where

import Data.Aeson (Value)
import Data.Text (Text)
import Servant.API
import Web.ArchLinux.Types

type GetPackageDetails =
  "packages"
    :> Capture "repository" Repo
    :> Capture "architecture" Arch
    :> Capture "package" Text
    :> "json"
    :> Get '[JSON] PackageInformation

type GetPackageFiles =
  "packages"
    :> Capture "repository" Repo
    :> Capture "architecture" Arch
    :> Capture "package" Text
    :> "files"
    :> "json"
    :> Get '[JSON] PackageFiles

type SearchPackage =
  "packages"
    :> "search"
    :> "json"
    :> QueryParam "q" Text
    :> QueryParam "name" Text
    :> QueryParam "desc" Text
    :> QueryParams "repo" Repo
    :> QueryParams "arch" Arch
    :> QueryParam "maintainer" Text
    :> QueryParam "packager" Text
    :> QueryParam "flagged" Flagged
    :> Get '[JSON] (ArchLinuxResponse PackageInformation)

type AurRPC =
  "rpc"
    :> QueryParam' '[Strict, Required] "v" Int
    :> QueryParam' '[Strict, Required] "type" Text
    :> QueryParam "by" Text
    :> QueryParams "arg" Text
    :> Get '[JSON] Value
