{-# LANGUAGE TemplateHaskellQuotes #-}

module Web.ArchLinux.Internal.KnownSPDXLoader (loadKnownSPDX) where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Map.Strict (Map, toList)
import GHC.Generics (Generic)
import Language.Haskell.TH
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

-- | Load the KNOWN_SPDX.json file at compile time and generate data type 'License', and
-- functions 'parseLicense' and 'licenseId'. If the license string is not known, 'parseLicense'
-- will return 'Custom' with the original string, and 'licenseId' will return the original string
-- for 'Custom' licenses.
loadKnownSPDX :: DecsQ
loadKnownSPDX = do
  txt <- runIO $ getCurrentDirectory >>= \dot -> BS.readFile $ dot </> "data" </> "KNOWN_SPDX.json"
  let (toList -> src) = case decodeStrict @(Map String String) txt of
        Just x -> x
        _ -> error "Failed to parse json"
  dec1 <- genData src
  dec2 <- genParse src
  dec3 <- genId src
  pure [dec1, dec2, dec3]

genData :: [(String, String)] -> DecQ
genData src = dataD_doc (pure []) (mkName "License") [] Nothing (customCon : (genCon <$> src)) drv (pure "SPDX License Identifiers included in @/usr/share/licenses/known_spdx_license_identifiers.txt@ of package <https://archlinux.org/packages/core/any/licenses/>.")
  where
    customCon = (normalC (mkName "Custom") [bangType (bang (pure NoSourceUnpackedness) (pure NoSourceStrictness)) [t|String|]], Nothing, [])
    genCon (from, to) = (normalC (mkName to) [], pure $ unwords ["@", from, "@"], [])
    drv = [derivClause (pure StockStrategy) [[t|Show|], [t|Eq|], [t|Ord|], [t|Generic|]]]

genParse :: [(String, String)] -> DecQ
genParse src = funD_doc (mkName "parseLicense") ((genClause <$> src) <> [custom]) (pure "Parse a SPDX license identifier. If the identifier is not known, return 'Custom' with the original string.") []
  where
    custom = clause [varP (mkName "s")] (normalB [|Custom s|]) []
    genClause (from, to) = clause [litP (StringL from)] (normalB $ conE (mkName to)) []

genId :: [(String, String)] -> DecQ
genId src = funD_doc (mkName "licenseId") ((genClause <$> src) <> [custom]) (pure "Get the SPDX license identifier. If the license is 'Custom', return the original string.") []
  where
    custom = clause [conP (mkName "Custom") [varP (mkName "s")]] (normalB [|s|]) []
    genClause (from, to) = clause [conP (mkName to) []] (normalB $ litE (StringL from)) []
