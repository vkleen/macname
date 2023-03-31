{ mkDerivation
, base
, bytestring
, containers
, cryptonite
, lib
, memory
, optparse-applicative
, text
, unix
}:
mkDerivation {
  pname = "macname";
  version = "0.1.0.0";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    bytestring
    containers
    cryptonite
    memory
    optparse-applicative
    text
    unix
  ];
  homepage = "https://github.com/githubuser/macname#readme";
  description = "Simple project template from stack";
  license = lib.licenses.bsd3;
  mainProgram = "macname";
}
