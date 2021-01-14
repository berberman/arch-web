{-# LANGUAGE TemplateHaskell #-}

-- | Copyright: (c) 2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- Lenses.
module Web.ArchLinux.Types.Lens
  ( module Web.ArchLinux.Types.Lens,
  )
where

import Control.Lens.TH (makeFieldsNoPrefix)
import Web.ArchLinux.Types

makeFieldsNoPrefix ''PackageInformation
makeFieldsNoPrefix ''PackageFiles
makeFieldsNoPrefix ''ArchLinuxResponse

makeFieldsNoPrefix ''AurSearch
makeFieldsNoPrefix ''AurInfo
makeFieldsNoPrefix ''AurResponse
