let
  pkgs = import <nixpkgs> { };
  drv = pkgs.haskellPackages.callPackage ./packages.nix { };
in
  drv