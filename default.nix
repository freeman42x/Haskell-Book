{ mkDerivation, base, QuickCheck, checkers, stdenv }:
mkDerivation {
  pname = "Haskell-Book";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base QuickCheck checkers ];
  homepage = "https://github.com/githubuser/Haskell-Book#readme";
  license = stdenv.lib.licenses.bsd3;
}
