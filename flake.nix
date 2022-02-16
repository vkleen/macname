{
  description = "macname";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
  };


  outputs = inputs@{ self, nixpkgs, ... }: let
    inherit (nixpkgs) lib;
    forAllSystems' = f: lib.mapAttrs f inputs.nixpkgs.legacyPackages;
    forAllSystems = f: lib.mapAttrs f pkgs;

    overlays = system:
      [
      (final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev: {
            basement = final.haskell.lib.dontHaddock hprev.basement;
            cryptonite = final.haskell.lib.dontHaddock hprev.cryptonite;
            optparse-applicative = final.haskell.lib.dontCheck hprev.optparse-applicative;
            QuickCheck = final.haskell.lib.dontCheck hprev.QuickCheck;
          };
        };
      })
      ];

    pkgs = forAllSystems' (system: pkgs: pkgs.appendOverlays (overlays system));

    ghcVersion = "921";

    pkg = _: p: {
      macname = p.haskell.packages."ghc${ghcVersion}".callCabal2nix "macname" "${self}" {};
    };

    devShell = system: p:
      p.mkShell {
        packages = [
          p.haskell.packages."ghc${ghcVersion}".cabal-install
        ];
        inputsFrom = [ self.packages."${system}".macname.env ];
      };

    idTableDrv = namespace: pkgs.x86_64-linux.runCommandNoCC "generate-hostid"
    { buildInputs = [
        self.packages.x86_64-linux.macname
        pkgs.x86_64-linux.coreutils
      ];
    } ''
      macname -q nix-table "${namespace}" > $out
    '';
  in {
    devShell = forAllSystems devShell;
    packages = forAllSystems pkg;

    idTable = lib.genAttrs [
      "wolkenheim.kleen.org"
      "auenheim.kleen.org"
    ] (ns: import (idTableDrv ns));

    defaultPackage = forAllSystems (s: _: self.packages."${s}".macname);
  };
}
