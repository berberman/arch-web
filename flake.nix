{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
      in with pkgs; {
        devShells.default = arch-web-dev.envFunc { withHoogle = true; };
        packages.default = arch-web;
      }) // {
        overlays.default = final: prev:
          let
            hpkgs = prev.haskellPackages;
            linkHaddockToHackage = drv:
              prev.haskell.lib.overrideCabal drv (drv: {
                haddockFlags = [
                  "--html-location='https://hackage.haskell.org/package/$pkg-$version/docs'"
                ];
              });
            arch-web = with prev.haskell.lib;
              linkHaddockToHackage (disableLibraryProfiling
                (dontCheck (hpkgs.callCabal2nix "arch-web" ./. { })));
          in with prev;
          with haskell.lib; {
            inherit arch-web;
            arch-web-dev =
              addBuildTools arch-web [ haskell-language-server cabal-install ];
          };
      };
}
