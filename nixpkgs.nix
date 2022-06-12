args@{ overlays ? [ ]
, commit ? "19574af0af3ffaf7c9e359744ed32556f34536bd"
, ...
}:
let
  githubTarball = owner: repo: rev:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    };
  easy-hls-src = githubTarball "blackheaven" "easy-hls-nix" "c0fa0e71b6f2d9923d4b965ee29a48d80b859309";
in
import (githubTarball "NixOS" "nixpkgs" commit) (args // {
  overlays = [
    (self: super: {
      easy-hls = self.callPackage easy-hls-src {
        ghcVersions = [ self.haskellPackages.ghc.version ];
      };
      ghcid = self.haskell.lib.justStaticExecutables self.haskellPackages.ghcid;
      hlint = self.haskell.lib.justStaticExecutables self.haskellPackages.hlint;
      ormolu = self.haskell.lib.justStaticExecutables self.haskellPackages.ormolu;
      hspec-discover = self.haskell.lib.justStaticExecutables self.haskellPackages.hspec-discover;

      compiler = self.haskellPackages.ghcWithHoogle (p:
        with p; [
        ]);
    })
  ];
})
