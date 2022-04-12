{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    llvm-hs = {
      url = "github:MuKnIO/llvm-hs/bytestring-0.11-pure";
      flake = false;
    };
    llvm-hs-pretty.url = "github:pnotequalnp/llvm-hs-pretty/llvm-12";
  };

  outputs = { self, nixpkgs, flake-utils, llvm-hs, llvm-hs-pretty }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hsFinal: hsPrev: {
            brainfuck = hsFinal.callCabal2nix "brainfuck" ./. { };
          };
        };
      };
    in flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        inherit (pkgs.lib) composeExtensions;
        lib = pkgs.haskell.lib;
        hs = pkgs.haskell.packages.ghc921.override {
          overrides = composeExtensions pkgs.haskell.packageOverrides
            (final: prev: {
              llvm-hs-pretty = lib.dontCheck
                (final.callCabal2nix "llvm-hs-pretty" llvm-hs-pretty { });
              llvm-hs-pure =
                final.callCabal2nix "llvm-hs-pure" "${llvm-hs}/llvm-hs-pure"
                { };
            });
        };
      in rec {
        packages = rec {
          brainfuck = hs.brainfuck;
          default = brainfuck;
        };

        apps = rec {
          brainfuck = flake-utils.lib.mkApp { drv = packages.brainfuck; };
          default = brainfuck;
        };

        devShells = {
          default = hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ brainfuck ];
            nativeBuildInputs = with hs; [
              cabal-install
              fourmolu
              haskell-language-server
              hlint
              pkgs.clang
              pkgs.llvmPackages_12.llvm
            ];
          };
          ci = hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ brainfuck ];
            nativeBuildInputs = with hs; [ cabal-install fourmolu pkgs.nixfmt ];
          };
        };
      }) // {
        overlays.default = overlay;
      };
}
