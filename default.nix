{ mkDerivation, base, QuickCheck, stdenv }:
mkDerivation {
  pname = "Haskell-Book";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/githubuser/Haskell-Book#readme";
  license = stdenv.lib.licenses.bsd3;
}
