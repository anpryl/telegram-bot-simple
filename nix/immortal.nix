{ mkDerivation
, base
, fetchgit
, lib
, stm
, tasty
, tasty-hunit
, transformers
, unliftio-core
}:
mkDerivation {
  pname = "immortal";
  version = "0.3";
  src = fetchgit {
    url = "https://github.com/UnkindPartition/immortal.git";
    sha256 = "1vw6rw6k5jlvjnbfs8vwfky1i5am6qd2nsna3npwffli1k5smwgz";
    rev = "7d61dd632af2bed33aecb5a8408932999a5057d3";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base stm unliftio-core ];
  testHaskellDepends = [ base stm tasty tasty-hunit transformers ];
  homepage = "https://github.com/feuerbach/immortal";
  description = "Spawn threads that never die (unless told to do so)";
  license = lib.licenses.mit;
}
