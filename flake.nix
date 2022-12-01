{
  description = "AoC";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "ghc924";

        pkgs = import nixpkgs { system = "${system}"; };
        haskellPkgs = pkgs.haskell.packages.${ghcVersion};
        ghc = haskellPkgs.ghcWithHoogle (_: []);
      in {
        packages = { };
        app = { };

        apps = {
          docs = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "docs";
              runtimeInputs = [ ghc ];
              text = "hoogle server --local --port 8000";
            };
          };

          update-flake = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "update-flake";
              text = ''
                nix flake update
                nix run .#backend-test
              '';
            };
          };
        };

        devShell = pkgs.mkShell {
          buildInputs = [
            ghc
            haskellPkgs.cabal-install
            haskellPkgs.haskell-language-server
            haskellPkgs.hlint
            haskellPkgs.ormolu

            pkgs.nixfmt
          ];
        };
      });
}
