let

    default =
        {
            ghcVersion = "ghc802";
            overrides = import ./overlay/haskell;
            srcFilter = p: t:
                baseNameOf p != "result"
                    && baseNameOf p != ".git"
                    && baseNameOf p != ".stack-work"
                    && t != "unknown";
            envMoreTools = nixpkgs: hs:
                [
                    (nixpkgs.callPackage (import ./tools/nix-tags-haskell) {})
                    (nixpkgs.callPackage (import ./tools/cabal-new-watch) {})
                    hs.cabal2nix
                    hs.cabal-install
                    hs.ghcid
                    hs.stylish-haskell
                    hs.hpack
                    hs.hlint
                ];
        };

in

{ nixpkgs
, pkgs
, ghcVersion ? default.ghcVersion
, overrides ? default.overrides
, srcFilter ? default.srcFilter
, envMoreTools ? default.envMoreTools
}:

let

    lib = import ./lib nixpkgs;

    ghc = builtins.getAttr ghcVersion nixpkgs.haskell.packages;

    mkDerivation = hPkgs:
        lib.nix.makeOverridable
            (args: hPkgs.mkDerivation args // { envArgs = args; });

    haskellPackages =
        ghc.override {
            overrides = self: super:
                (overrides nixpkgs self super)
                    // { mkDerivation = mkDerivation super; }
                    // pkgs;
        };

    name-from = let findCabal =
                      lib.nix.composed [
                          (lib.nix.findFirst
                              (lib.nix.hasSuffix ".cabal")
                              ("error"))
                          builtins.attrNames
                          builtins.readDir
                      ];
                in p: lib.nix.composed [
                     (lib.nix.removeSuffix ".cabal")
                     (x:
                         if x == "error"
                         then throw ("no Cabal file found: " + p)
                         else x)
                     (x: if x == "error" then findCabal p else x)
                     findCabal
                     dirOf
                   ] p;

    callHaskell = p:
        lib.nix.composed
            [ lib.haskell.dontHaddock (lib.haskell.filterSource srcFilter) ]
            (haskellPackages.callCabal2nix (name-from p) p {});

    callHaskellApp = p: lib.haskell.disableSharedExecutables (callHaskell p);

    envPkgs =
       builtins.filter
           (e: builtins.isAttrs e && e ? envArgs)
           (builtins.attrValues pkgs);

    envFilter = drv:
        drv != null && ! builtins.elem drv (builtins.attrValues pkgs);

    envArg = a:
        lib.nix.filter envFilter
            (lib.nix.unique
                (builtins.foldl'
                    (acc: s:
                        if builtins.hasAttr a s.envArgs
                        then builtins.getAttr a s.envArgs ++ acc
                        else acc)
                    []
                    envPkgs));

    env =
        (haskellPackages.mkDerivation {

            pname = "env-haskell";
            version = "0.0.1.0";
            license = lib.nix.licenses.bsd3;

            buildDepends = envArg "buildDepends";
            setupHaskellDepends = envArg "setupHaskellDepends";
            libraryHaskellDepends = envArg "libraryHaskellDepends";
            executableHaskellDepends = envArg "executableHaskellDepends";

            buildTools = (envMoreTools nixpkgs haskellPackages) ++ envArg "buildTools";
            libraryToolDepends = envArg "libraryToolDepends";
            executableToolDepends = envArg "executableToolDepends";
            testToolDepends = envArg "testToolDepends";

        }).env;

in

{
    inherit haskellPackages callHaskellApp env;
    callHaskellLib = callHaskell;
}
