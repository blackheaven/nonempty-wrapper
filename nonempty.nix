{ mkDerivation
, lib
, base
, hspec
, hspec-core
}:
mkDerivation {
  pname = "nonempty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec hspec-core ];
  librarySystemDepends = [ ];
  libraryPkgconfigDepends = [ ];
  license = lib.licenses.bsd2;
  hydraPlatforms = lib.platforms.none;
}
