# archlinux-web

[![Hackage](https://img.shields.io/hackage/v/archlinux-web.svg?logo=haskell)](https://hackage.haskell.org/package/archlinux-web)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
![build](https://github.com/berberman/archlinux-web/workflows/build/badge.svg)


`archlinux-web` is a simple library providing functions to access [Official repositories web interface](https://wiki.archlinux.org/index.php/Official_repositories_web_interface) and [Aurweb RPC interface](https://wiki.archlinux.org/index.php/Aurweb_RPC_interface), based on [servant-client](https://hackage.haskell.org/package/servant-client).

## Documentation

Documentation of released version is available at [hackage](https://hackage.haskell.org/package/archlinux-web),
and of master is at [github pages](https://berberman.github.io/archlinux-web).

## Example

* Print [linux](https://archlinux.org/packages/core/x86_64/linux/)'s version:

```haskell
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Web.ArchLinux
import Web.ArchLinux.Types.Lens

main :: IO ()
main = void . runAPIClient' $ do
  linux <- getPackageDetails Core X86_64 "linux"
  liftIO . putStrLn $ "linux in [core] has version: " <> T.unpack (linux ^. pkgver)
```

* Search keywords "yay":

```haskell
import Control.Lens 
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Web.ArchLinux
import Web.ArchLinux.Types.Lens

main :: IO ()
main = void . runAPIClient' $ do
  response <- searchAur ByNameOrDesc "yay"
  liftIO . print $ (response ^. results ^.. each . name)
```

* ...

