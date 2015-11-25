{ mkDerivation, base, bytestring, parsec, process, stdenv
, stringsearch, text, unix
}:
mkDerivation {
  pname = "jcoreutils";
  version = "1.1.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring parsec process stringsearch text unix
  ];
  homepage = "https://github.com/jekor/jcoreutils";
  description = "additions and enhancements to GNU Core Utilities";
  license = "MIT";
}
