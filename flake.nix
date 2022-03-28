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
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlay = self: super: {
          haskell = super.haskell // {
            packageOverrides = hsSelf: hsSuper: {
              brainfuck = hsSelf.callCabal2nix "brainfuck" ./. { };
              llvm-hs-pretty = self.haskell.lib.dontCheck (hsSelf.callCabal2nix "llvm-hs-pretty" llvm-hs-pretty { });
              llvm-hs-pure = hsSelf.callCabal2nix "llvm-hs-pure" "${llvm-hs}/llvm-hs-pure" { };
            };
          };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        hs = pkgs.haskell.packages.ghc921;
      in rec {
        inherit overlay;

        packages = rec {
          brainfuck = hs.brainfuck;
          default = brainfuck;
        };

        apps = rec {
          brainfuck = flake-utils.lib.mkApp { drv = packages.brainfuck; };
          default = brainfuck;
        };

        # Compatibility for older Nix versions
        defaultApp = apps.default;
        defaultPackage = packages.default;

        devShell = hs.shellFor {
          packages = hsPkgs: with hsPkgs; [ brainfuck ];
          nativeBuildInputs = with hs; [
            cabal-install
            fourmolu
            haskell-language-server
            hlint
          ];
        };
      });
}
