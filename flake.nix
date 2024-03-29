{
  description = "macname";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    nix-filter.url = "github:numtide/nix-filter";
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
                # basement = final.haskell.lib.dontHaddock hprev.basement;
                # cryptonite = final.haskell.lib.dontHaddock hprev.cryptonite;
                # optparse-applicative = final.haskell.lib.dontCheck hprev.optparse-applicative;
                # QuickCheck = final.haskell.lib.dontCheck hprev.QuickCheck;
              };
            };
          })
        ];

      ghcVersion = "94";

      macnameOverlay = final: prev: {
        macname = final.haskell.lib.overrideCabal
          (final.haskell.packages."ghc${ghcVersion}".callPackage ./nix/macname.nix { })
          (_: { inherit src; });
      };

      pkgs = forAllSystems' (system: pkgs: pkgs.extend macnameOverlay);

      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "macname.cabal"
          "LICENSE"
          "Setup.hs"
          "src"
        ];
      };


      pkg = _: p: rec {
        idTable = idTableDrv p macname;
        elementTable = elementTableDrv p macname;
        macname-deriver = (p.haskell.packages."ghc${ghcVersion}".callCabal2nix "macname" src { }).cabal2nixDeriver;
        inherit (p) macname;
      };

      devShell = system: p:
        p.mkShell {
          packages = [
            p.haskell.packages."ghc${ghcVersion}".cabal-install
          ];
          inputsFrom = [ self.packages."${system}".macname.env ];
        };


      namespaces = [
        "wolkenheim.kleen.org"
        "auenheim.kleen.org"
        "nixos-installers.kleen.org"
      ];

      idTableDrv = p: macname: p.runCommandNoCC "generate-hostid-table"
        {
          buildInputs = [
            macname
          ];
        } ''
        mkdir $out
        ${lib.concatMapStrings
          (namespace: ''
            macname -q nix-table "${namespace}" > $out/"${namespace}".nix
          '')
          namespaces
        }
      '';

      elementTableDrv = p: macname: p.runCommandNoCC "generate-element-table"
        {
          buildInputs = [
            macname
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
        self.idTable.${namespace}.${element} or (
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
      overlays.default = macnameOverlay;
      packages = forAllSystems pkg;

      idTable = lib.listToAttrs (builtins.map (ns: lib.nameValuePair ns (import ./nix/id-table/${ns}.nix)) namespaces);
      elementTable = import ./nix/element-table.nix;

      computeHostId = forAllSystems computeHostId;
      computeLinkId = forAllSystems computeLinkId;
      computeHash16 = forAllSystems computeHash16;

      defaultPackage = forAllSystems (s: _: self.packages."${s}".macname);
    };
}
