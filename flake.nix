{
  description = "macname";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
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

      ghcVersion = "924";

      pkg = _: p: {
        macname = p.haskell.packages."ghc${ghcVersion}".callCabal2nix "macname" "${self}" { };
      };

      devShell = system: p:
        p.mkShell {
          packages = [
            p.haskell.packages."ghc${ghcVersion}".cabal-install
          ];
          inputsFrom = [ self.packages."${system}".macname.env ];
        };

      idTableDrv = system: p: namespace: p.runCommandNoCC "generate-hostid-table"
        {
          buildInputs = [
            self.packages.${system}.macname
          ];
        } ''
        macname -q nix-table "${namespace}" > $out
      '';

      elementTableDrv = system: p: p.runCommandNoCC "generate-element-table"
        {
          buildInputs = [
            self.packages.${system}.macname
          ];
        } ''
        macname -q element-table > $out
      '';

      # A link is a set containing attributes "to" and "from", each of which is a set { host = ...; intf = ...; }
      computeLinkId = system: p: l:
        hexToInt (builtins.fromJSON (builtins.readFile (p.runCommandNoCC "compute-link-id"
          {
            buildInputs = [
              self.packages.${system}.macname
              p.coreutils
            ];
          } ''
          printf '"%s"\n' "$(macname -q link-id "${l.from.host}:${l.from.intf} - ${l.to.host}:${l.to.intf}")" > $out
        '')));

      computeHash16 = system: p: d:
        hexToInt (builtins.fromJSON (builtins.readFile (p.runCommandNoCC "compute-hash16"
          {
            buildInputs = [
              self.packages.${system}.macname
              p.coreutils
            ];
          } ''
          printf '"%s"\n' "$(macname -q hash16 "${d}")" > $out
        '')));

      computeHostId = system: p: namespace: element:
        self.idTable.${system}.${namespace}.${element} or (
          builtins.fromJSON (builtins.readFile (p.runCommandNoCC "generate-hostid"
            {
              buildInputs = [
                self.packages.${system}.macname
                p.coreutils
              ];
            } ''
            printf '"%s"\n' "$(macname -q search "${namespace}" "${element}")" > $out
          '')));
    in
    {
      devShell = forAllSystems devShell;
      packages = forAllSystems pkg;

      idTable = forAllSystems (s: p: lib.genAttrs [
        "wolkenheim.kleen.org"
        "auenheim.kleen.org"
        "nixos-installers.kleen.org"
      ]
        (ns: import (idTableDrv s p ns)));

      elementTable = forAllSystems (s: p: import (elementTableDrv s p));

      computeHostId = forAllSystems computeHostId;
      computeLinkId = forAllSystems computeLinkId;
      computeHash16 = forAllSystems computeHash16;

      defaultPackage = forAllSystems (s: _: self.packages."${s}".macname);
    };
}
