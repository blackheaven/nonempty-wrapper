{
  description = "nonempty";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc923;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
      in
      rec
      {
        packages.nonempty = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "nonempty" ./nonempty rec {
            # Dependency overrides go here
          };
        packages.nonempty-quickcheck = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "nonempty-quickcheck" ./nonempty-quickcheck rec {
            # Dependency overrides go here
            nonempty = packages.nonempty;
          };
        packages.nonempty-aeson = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "nonempty-aeson" ./nonempty-aeson rec {
            # Dependency overrides go here
            nonempty = packages.nonempty;
          };

        defaultPackage = pkgs.linkFarmFromDrvs "all-nonempty" (pkgs.lib.unique (builtins.attrValues packages));


        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
