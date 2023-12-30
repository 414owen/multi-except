{ mkDerivation, base, hspec, lib, semigroupoids }:
mkDerivation {
  pname = "multi-except";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base semigroupoids ];
  testHaskellDepends = [ base hspec semigroupoids ];
  doHaddock = false;
  homepage = "https://github.com/414owen/multi-except";
  description = "Multiple Exceptions";
  license = lib.licenses.mit;
}
