{ pkgs-func ? import (import ./fetch-nixpkgs.nix), compiler ? "ghc844" }:
let
  overlays = [];
  pkgs = pkgs-func {
    inherit overlays;
  };
  composed = builtins.foldl' (a: acc: b: a (acc b)) (a: a);
in with pkgs.lib; with pkgs.haskell.lib;
let
  hsPkgs = pkgs.haskell.packages."${compiler}".extend (self: super: {
    haddock-api = dontHaddock (doJailbreak super.haddock-api);
  });
in hsPkgs.dash-haskell
