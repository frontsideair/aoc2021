{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            ${packageName} =
              hself.callCabal2nix packageName ./. { };
          };
        };

        packageName = "solution";
      in {
        packages.${packageName} = haskellPackages.${packageName};

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          packages = p: [p.${packageName}];
          buildInputs = [haskellPackages.haskell-language-server haskellPackages.cabal-install haskellPackages.ghcid];
        };
      });
}
