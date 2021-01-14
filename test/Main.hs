module Main (main) where

import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Test.HUnit
import Web.ArchLinux

main :: IO ()
main = do
  manager <- newTlsManager
  runTestTTAndExit $ tests manager

tests :: Manager -> Test
tests manager =
  TestList
    [ createTest "getPackageDetails" manager $ getPackageDetails Core X86_64 "linux",
      createTest "getPackageFiles" manager $ getPackageFiles Core X86_64 "linux",
      createTest "searchPackage" manager $ searchPackage emptySearchOptions {_nameOrDescription = Just "linux"},
      createTest "searchAur" manager $ searchAur ByNameOrDesc "haskell",
      createTest "getAurInfo" manager $ getAurInfo ["arch-hs-git"]
    ]

createTest :: HasBaseUrl k => String -> Manager -> APIClient k a -> Test
createTest tag manager action = TestLabel tag . TestCase $ do
  result <- runAPIClient manager action
  case result of
    Right _ -> pure ()
    Left err -> assertFailure $ show err
