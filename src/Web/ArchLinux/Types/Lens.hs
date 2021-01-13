{-# LANGUAGE TemplateHaskell #-}

module Web.ArchLinux.Types.Lens where

import Control.Lens.TH (makeFields)
import Web.ArchLinux.Types

makeFields ''PackageInformation
makeFields ''PackageFiles
makeFields ''ArchLinuxResponse

makeFields ''AurSearch
makeFields ''AurInfo
makeFields ''AurResponse
