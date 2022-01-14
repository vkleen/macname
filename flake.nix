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
  in {
    devShell = forAllSystems devShell;
    packages = forAllSystems pkg;

    defaultPackage = forAllSystems (s: _: self.packages."${s}".macname);
  };
}
