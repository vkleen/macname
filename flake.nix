{
  description = "macname";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stackageSrc = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
    hackageSrc = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };


  outputs = inputs@{ self, nixpkgs, haskell-nix, ... }: let
    inherit (nixpkgs) lib;
    forAllSystems' = f: lib.mapAttrs f inputs.nixpkgs.legacyPackages;
    forAllSystems = f: lib.mapAttrs f pkgs;

    overlays = system:
      [ (haskell-nix.overlays.combined)
        (final: prev: {
          evalPackages = (import final.path {
            overlays = [haskell-nix.overlays.combined];
            localSystem = system;
          }).buildPackages;
        })
        (final: prev: {
          haskell-nix = prev.haskell-nix // {
            sources = prev.haskell-nix.sources // {
              hackage = inputs.hackageSrc;
              stackage = inputs.stackageSrc;
            };
          };
        })
      ];

    pkgs = forAllSystems' (system: pkgs: pkgs.appendOverlays (overlays system));

    haskell' = p:
      p.haskell-nix.project {
        src = ./.;
        sha256map = {
        };
        compiler-nix-name = "ghc8107";
        # materialized = ./materialized;
        # index-state = "2021-02-23T00:00:00Z";
      };

    haskell = forAllSystems (_: p: haskell' p);

    devShell = forAllSystems (system: p:
      haskell.${system}.shellFor {
        packages = ps: with ps; [
          macname
        ];
        tools = {
          haskell-language-server = "latest";
          cabal = "latest";
          hpack = "latest";
          c2hs = "latest";
          ghcid = "latest";
          fourmolu = "latest";
        };
        buildInputs = [
        ];

        withHoogle = false;

        exactDeps = true;
      });
  in {
    inherit devShell;

    packages = forAllSystems (system: _: {
      macname = haskell."${system}".hsPkgs.macname.components.exes.macname;
    });
  };
}
