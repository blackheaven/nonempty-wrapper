let
  pkgs = import ./nixpkgs.nix { };
in
pkgs.mkShell {
  packages = with pkgs; [
    easy-hls
    ghc
    ghcid
    cabal-install
    hlint
    hspec-discover
    nixpkgs-fmt
  ];
}
