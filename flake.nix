{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
      in with pkgs; {
        devShell = arch-web-dev.envFunc { withHoogle = true; };
        defaultPackage = arch-web;
      }) // {
        overlay = self: super:
          let
            hpkgs = super.haskellPackages;
            linkHaddockToHackage = drv:
              super.haskell.lib.overrideCabal drv (drv: {
                haddockFlags = [
                  "--html-location='https://hackage.haskell.org/package/$pkg-$version/docs'"
                ];
              });
            arch-web = with super.haskell.lib;
              linkHaddockToHackage (disableLibraryProfiling
                (dontCheck (hpkgs.callCabal2nix "arch-web" ./. { })));
          in with super;
          with haskell.lib; {
            inherit arch-web;
            arch-web-dev = addBuildTools arch-web [
              haskell-language-server
              cabal-install
            ];
          };
      };
}
