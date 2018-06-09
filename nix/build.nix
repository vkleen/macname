{ pkgs ? import (import ./fetch-nixpkgs.nix) {}, compiler ? "ghc842" }:

with pkgs.lib; with pkgs.haskell.lib;
let
  pkgsMake = import ./pkgs-make {
    origNixpkgs = pkgs;
    haskellArgs = {
      ghcVersion = compiler;
    };
  };
in pkgsMake ({call, lib} :
  let
    modifiedHaskellCall = f:
      lib.nix.composed [
        lib.haskell.enableLibraryProfiling
        lib.haskell.doHaddock
        f
      ];
    haskellLib = modifiedHaskellCall call.haskell.lib;
    haskellApp = modifiedHaskellCall call.haskell.app;
  in rec {
    macname             = haskellApp ../.;
  })
