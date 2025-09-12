{
  description = "dev env for hasql-mover";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ flake-utils.overlay ];
      };

      ghc = "ghc984";
      fs = pkgs.lib.fileset;
      hlib = pkgs.haskell.lib;
      packageName = "hasql-mover";

      hp = with hlib; pkgs.haskell.packages.${ghc}.override {
        overrides = hp: super: {
          ${packageName} = (justStaticExecutables (disableLibraryProfiling (disableExecutableProfiling (dontHaddock (hp.callCabal2nix packageName (fs.toSource {
            root = ./.;
            fileset = fs.unions [ ./src ./${packageName}.cabal ];
          }) {})))));
        };
      };

      pkg = hp.${packageName};

    in flake-utils.lib.eachSystem [ system ] (system: {
      packages = {
        default = pkg;
      };
      defaultPackage = self.packages.${system}.default;
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          hp.haskell-language-server
          hp.fourmolu
          hp.cabal-fmt
          hp.ghcid
          hp.cabal-install
          hp.hlint
          treefmt
          just
        ];
        inputsFrom = map (e: (e.env or {})) (__attrValues self.packages.${system});
      };
      devShell = self.devShells.${system}.default;
    });
}
