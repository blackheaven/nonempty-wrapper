{ nixpkgs ? import ./nixpkgs.nix { }, compiler ? "ghc8107" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./nonempty.nix { }
