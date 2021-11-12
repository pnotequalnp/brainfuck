{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hs = pkgs.haskellPackages;
      in rec {
        packages.brainfuck = hs.callCabal2nix "brainfuck" ./. { };
        defaultPackage = packages.brainfuck;

        apps.brainfuck = flake-utils.lib.mkApp { drv = packages.brainfuck; };
        defaultApp = apps.brainfuck;

        devShell = packages.brainfuck.env.overrideAttrs
          (super: { buildInputs = super.buildInputs ++ [ hs.cabal-install ]; });
      });
}
