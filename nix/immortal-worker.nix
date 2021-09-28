{ mkDerivation
, base
, deepseq
, fetchgit
, immortal
, lib
, monad-logger
, safe-exceptions
, text
, unliftio-core
}:
mkDerivation {
  pname = "immortal-worker";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/NCrashed/immortal-worker.git";
    sha256 = "1pjxkzhzyzriryi4qdm6vn9ywchwcx1ccr92lgzy4xyh6xsgp22i";
    rev = "be3e5016f6ff529ff6634bbd1c958625bb4ae243";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base
    deepseq
    immortal
    monad-logger
    safe-exceptions
    text
    unliftio-core
  ];
  description = "Create worker threads that logs exceptions and restarts";
  license = lib.licenses.mit;
}
