{ mkDerivation, base, dlist-nonempty, lib, semigroupoids, hspec }:
mkDerivation {
  pname = "multi-except";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [ base dlist-nonempty semigroupoids ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/414owen/multi-except";
  description = "Multiple Exceptions";
  license = lib.licenses.mit;
}
