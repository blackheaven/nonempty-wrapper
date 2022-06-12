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
      {
        packages.nonempty = # (ref:haskell-package-def)
          haskellPackages.callCabal2nix "nonempty" ./nonempty rec {
            # Dependency overrides go here
          };

        defaultPackage = pkgs.stdenv.mkDerivation {
          name = "nonempty";
          src = self;
          phases = [ "installPhase" ];
          installPhase = ''touch $out/done'';
          buildInputs = [
            self.packages.${system}.nonempty
          ];
        };

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
