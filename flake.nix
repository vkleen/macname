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
                basement = final.haskell.lib.dontHaddock hprev.basement;
                cryptonite = final.haskell.lib.dontHaddock hprev.cryptonite;
                optparse-applicative = final.haskell.lib.dontCheck hprev.optparse-applicative;
                QuickCheck = final.haskell.lib.dontCheck hprev.QuickCheck;
              };
            };
          })
        ];

      pkgs = forAllSystems' (system: pkgs: pkgs.appendOverlays (overlays system));

      ghcVersion = "961";

      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "macname.cabal"
          "LICENSE"
          "Setup.hs"
          "src"
        ];
      };


      pkg = _: p: {
        macname-deriver = (p.haskell.packages."ghc${ghcVersion}".callCabal2nix "macname" src { }).cabal2nixDeriver;
        macname = p.haskell.lib.overrideCabal
          (p.haskell.packages."ghc${ghcVersion}".callPackage ./macname.nix { })
          (_: { inherit src; });
      };

      devShell = system: p:
        p.mkShell {
          packages = [
            p.haskell.packages."ghc${ghcVersion}".cabal-install
          ];
          inputsFrom = [ self.packages."${system}".macname.env ];
        };


      namespaceOutputHashes = {
        "wolkenheim.kleen.org" = "sha256-3TAAkXRdeolU++i79zyWmD5ZiLlZ/WNwus+A9MAXaQw=";
        "auenheim.kleen.org" = "sha256-zsdgYcXthIQ63CDlh9Wq+EJsn3Kk9oNuIu6MvocPqi4=";
        "nixos-installers.kleen.org" = "sha256-YJ++gdGGlF5GRWBWEhHpzWseFJp8pppxbcNUHnf5URw=";
      };

      idTableDrv = system: p: namespace: outputHash: p.runCommandNoCC "generate-hostid-table"
        {
          buildInputs = [
            self.packages.${system}.macname
          ];

          outputHashMode = "flat";
          outputHashAlgo = "sha256";
          inherit outputHash;
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

      idTable = forAllSystems (s: p: lib.mapAttrs
        (ns: hash: import (idTableDrv s p ns hash))
        namespaceOutputHashes);

      idTableDrv = forAllSystems (s: p: lib.mapAttrs
        (ns: hash: idTableDrv s p ns hash)
        namespaceOutputHashes);

      elementTable = forAllSystems (s: p: import (elementTableDrv s p));

      computeHostId = forAllSystems computeHostId;
      computeLinkId = forAllSystems computeLinkId;
      computeHash16 = forAllSystems computeHash16;

      defaultPackage = forAllSystems (s: _: self.packages."${s}".macname);
    };
}
