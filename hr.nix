{ mkDerivation, ansi-wl-pprint, base, optparse-applicative, stdenv
, terminal-size, time
}:
mkDerivation {
  pname = "hr";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    ansi-wl-pprint base optparse-applicative terminal-size time
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/ExtremaIS/hr-haskell#readme";
  description = "horizontal rule for the terminal";
  license = stdenv.lib.licenses.mit;
}
