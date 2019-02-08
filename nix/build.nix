{ pkgs-func ? import (import ./fetch-nixpkgs.nix), compiler ? "ghc863" }:

let
  overlays = [];
  pkgs = pkgs-func {
    inherit overlays;
  };
  composed = builtins.foldl' (a: acc: b: a (acc b)) (a: a);
in with pkgs.lib; with pkgs.haskell.lib;
let
  enable = x: drv: enableCabalFlag drv x;
  disable = x: drv: disableCabalFlag drv x;
  stdhsPackages = pkgs.haskell.packages."${compiler}".extend (self: super: {
  });
  haskellPackages = stdhsPackages.extend (self: super: {
    macname      = (self.callCabal2nixWithOptions "macname" ../. "" {});
  });
in rec {
  inherit haskellPackages;
  inherit (haskellPackages) macname;
  shell = haskellPackages.shellFor {
    packages = p: with p; [ macname ];
    withHoogle = true;
    buildInputs = with haskellPackages; [
      cabal2nix cabal-install ghcid stylish-haskell hpack hlint
    ];
  };
}
