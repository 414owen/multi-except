{ mkDerivation, base, dlist_1_0, lib, semigroupoids, hspec }:
mkDerivation {
  pname = "multi-except";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [ base dlist_1_0 semigroupoids ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/414owen/multi-except";
  description = "Multiple Exceptions";
  license = lib.licenses.mit;
}
