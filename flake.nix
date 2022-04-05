{
  description = "macname";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
  };

  outputs = inputs@{ self, nixpkgs, ... }: let
    inherit (nixpkgs) lib;
    inherit (import ./hex-to-int.nix { inherit lib; }) hexToInt;
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

    ghcVersion = "922";

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

    idTableDrv = namespace: pkgs.x86_64-linux.runCommandNoCC "generate-hostid-table"
    { buildInputs = [
        self.packages.x86_64-linux.macname
      ];
    } ''
      macname -q nix-table "${namespace}" > $out
    '';

    elementTableDrv = pkgs.x86_64-linux.runCommandNoCC "generate-element-table"
    { buildInputs = [
        self.packages.x86_64-linux.macname
      ];
    } ''
      macname -q element-table > $out
    '';

    # A link is a set containing attributes "to" and "from", each of which is a set { host = ...; intf = ...; }
    computeLinkId = l:
      hexToInt (builtins.fromJSON (builtins.readFile (pkgs.x86_64-linux.runCommandNoCC "compute-link-id"
        { buildInputs = [
            self.packages.x86_64-linux.macname
            pkgs.x86_64-linux.coreutils
          ];
        } ''
          printf '"%s"\n' "$(macname -q link-id "${l.from.host}:${l.from.intf} - ${l.to.host}:${l.to.intf}")" > $out
        '')));

    computeHash16 = d:
      hexToInt (builtins.fromJSON (builtins.readFile (pkgs.x86_64-linux.runCommandNoCC "compute-hash16"
        { buildInputs = [
            self.packages.x86_64-linux.macname
            pkgs.x86_64-linux.coreutils
          ];
        } ''
          printf '"%s"\n' "$(macname -q hash16 "${d}")" > $out
        '')));

    computeHostId = namespace: element:
      self.idTable.${namespace}.${element} or (
        builtins.fromJSON (builtins.readFile (pkgs.x86_64-linux.runCommandNoCC "generate-hostid"
          { buildInputs = [
              self.packages.x86_64-linux.macname
              pkgs.x86_64-linux.coreutils
            ];
          } ''
            printf '"%s"\n' "$(macname -q search "${namespace}" "${element}")" > $out
          '')));
  in {
    devShell = forAllSystems devShell;
    packages = forAllSystems pkg;

    idTable = lib.genAttrs [
      "wolkenheim.kleen.org"
      "auenheim.kleen.org"
    ] (ns: import (idTableDrv ns));

    elementTable = import elementTableDrv;
    
    inherit computeHostId computeLinkId computeHash16;

    defaultPackage = forAllSystems (s: _: self.packages."${s}".macname);
  };
}
