module Main (main) where

import Control.Exception (SomeException, catch)
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

createTest :: (HasBaseUrl k) => String -> Manager -> APIClient k a -> Test
createTest tag manager action = TestLabel tag . TestCase $ do
  -- Retry up to 3 times in case of network errors or rate limiting
  result <- retry 3 $ runAPIClient manager action
  case result of
    Right _ -> pure ()
    Left err -> assertFailure $ show err

retry :: Int -> IO a -> IO a
retry 0 action = action
retry n action = action `catch` \(_ :: SomeException) -> retry (n - 1) action
