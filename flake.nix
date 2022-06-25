{
  description = "nonempty-wrapper";

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
        packages.nonempty-wrapper = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "nonempty-wrapper" ./nonempty-wrapper rec {
            # Dependency overrides go here
          };
        packages.nonempty-wrapper-quickcheck = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "nonempty-wrapper-quickcheck" ./nonempty-wrapper-quickcheck rec {
            # Dependency overrides go here
            nonempty-wrapper = packages.nonempty-wrapper;
          };
        packages.nonempty-wrapper-aeson = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "nonempty-wrapper-aeson" ./nonempty-wrapper-aeson rec {
            # Dependency overrides go here
            nonempty-wrapper = packages.nonempty-wrapper;
          };
        packages.nonempty-wrapper-text = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "nonempty-wrapper-text" ./nonempty-wrapper-text rec {
            # Dependency overrides go here
            nonempty-wrapper = packages.nonempty-wrapper;
          };

        defaultPackage = pkgs.linkFarmFromDrvs "all-nonempty-wrapper" (pkgs.lib.unique (builtins.attrValues packages));


        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            haskell-ci
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
