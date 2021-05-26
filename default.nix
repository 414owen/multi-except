{ mkDerivation
, stdenv
, base
, dlist_1_0
, semigroupoids
, haskell-language-server
}:

mkDerivation {
  pname = "multi-except";
  version = "0.1.2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    dlist_1_0
    semigroupoids
  ];
  buildTools = [
    haskell-language-server
  ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
