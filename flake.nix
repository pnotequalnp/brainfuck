{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hs = pkgs.haskell.packages.ghc921.override {
          overrides = self: super: {
            # Bytestring is currently at 0.11 and the llvm-hs family still wants 0.10
            llvm-hs-pure = pkgs.haskell.lib.doJailbreak super.llvm-hs-pure;
          };
        };
      in
      rec {
        packages.brainfuck = hs.callCabal2nix "brainfuck" ./. { };
        defaultPackage = packages.brainfuck;

        apps.brainfuck = flake-utils.lib.mkApp { drv = packages.brainfuck; };
        defaultApp = apps.brainfuck;

        devShell = packages.brainfuck.env.overrideAttrs
          (super: {
            buildInputs = with hs; super.buildInputs ++ [
              cabal-install
              fourmolu
              haskell-language-server
              hlint
            ];
          });
      });
}
