{
  description = "Configurable brainfuck interpreter and optimizing compiler";

  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (pkgs.lib) composeExtensions;
      inherit (pkgs.haskell.lib.compose) unmarkBroken;
      hs = pkgs.haskell.packages.ghc8107.override {
        overrides = composeExtensions pkgs.haskell.packageOverrides
          (final: prev: { llvm-hs = unmarkBroken (prev.llvm-hs); });
      };
      brainfuck = hs.callCabal2nix "brainfuck" ./. { };
    in {
      packages.${system} = {
        default = brainfuck;
        brainfuck = brainfuck;
      };

      apps.${system} = rec {
        default = brainfuck;
        brainfuck = {
          type = "app";
          program = "${self.packages.${system}.brainfuck}/bin/brainfuck";
        };
      };

      devShells.${system} = {
        default = hs.shellFor {
          packages = p: [ brainfuck ];
          nativeBuildInputs = with hs; [
            cabal-install
            fourmolu
            haskell-language-server
            hlint
          ];
        };
        ci = hs.shellFor {
          packages = p: [ ];
          nativeBuildInputs = with hs; [ cabal-install fourmolu ];
        };
      };
    };
}
