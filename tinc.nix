{ nixpkgs }:
rec {
  compiler = nixpkgs.haskellPackages;
  resolver =
    let
      callPackage = compiler.callPackage;

      overrideFunction = self: super: rec {
        base-orphans = callPackage
          (
            { mkDerivation, base, ghc-prim, hspec, hspec-discover, QuickCheck
            , stdenv
            }:
            mkDerivation {
              pname = "base-orphans";
              version = "0.7";
              sha256 = "0aaddc39e3d0bba13acfcf0009ef57bf91d2ee74b295041d63e14c6caf4dee14";
              libraryHaskellDepends = [ base ghc-prim ];
              testHaskellDepends = [ base hspec QuickCheck ];
              testToolDepends = [ hspec-discover ];
              homepage = "https://github.com/haskell-compat/base-orphans#readme";
              description = "Backwards-compatible orphan instances for base";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck; };
        cabal-doctest = callPackage
          (
            { mkDerivation, base, Cabal, directory, filepath, stdenv }:
            mkDerivation {
              pname = "cabal-doctest";
              version = "1.0.6";
              sha256 = "decaaa5a73eaabaf3c4f8c644bd7f6e3f428b6244e935c0cf105f75f9b24ed2d";
              revision = "1";
              editedCabalFile = "1bk85avgc93yvcggwbk01fy8nvg6753wgmaanhkry0hz55h7mpld";
              libraryHaskellDepends = [ base Cabal directory filepath ];
              homepage = "https://github.com/phadej/cabal-doctest";
              description = "A Setup.hs helper for doctests running";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        random = callPackage
          (
            { mkDerivation, base, stdenv, time }:
            mkDerivation {
              pname = "random";
              version = "1.1";
              sha256 = "b718a41057e25a3a71df693ab0fe2263d492e759679b3c2fea6ea33b171d3a5a";
              revision = "1";
              editedCabalFile = "1pv5d7bm2rgap7llp5vjsplrg048gvf0226y0v19gpvdsx7n4rvv";
              libraryHaskellDepends = [ base time ];
              testHaskellDepends = [ base ];
              description = "random number library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        semigroups = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "semigroups";
              version = "0.18.4";
              sha256 = "589e3042329a6bcffb5c0e85834143586db22eb7a2aae094d492cd004f685d27";
              libraryHaskellDepends = [ base ];
              homepage = "http://github.com/ekmett/semigroups/";
              description = "Anything that associates";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        stm = callPackage
          (
            { mkDerivation, array, base, stdenv }:
            mkDerivation {
              pname = "stm";
              version = "2.4.5.0";
              sha256 = "31d7db183f13beed5c71409d12747a7f4cf3e145630553dc86336208540859a7";
              libraryHaskellDepends = [ array base ];
              homepage = "https://wiki.haskell.org/Software_transactional_memory";
              description = "Software Transactional Memory";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        text = callPackage
          (
            { mkDerivation, array, base, binary, bytestring, deepseq, directory
            , ghc-prim, HUnit, integer-gmp, QuickCheck, quickcheck-unicode
            , random, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "text";
              version = "1.2.3.0";
              sha256 = "20e0b1627f613b32cc7f2d2e8dcc48a4a61938b24f3d14fb77cee694f0c9311a";
              libraryHaskellDepends = [
                array base binary bytestring deepseq ghc-prim integer-gmp
              ];
              testHaskellDepends = [
                array base binary bytestring deepseq directory ghc-prim HUnit
                integer-gmp QuickCheck quickcheck-unicode random test-framework
                test-framework-hunit test-framework-quickcheck2
              ];
              doCheck = false;
              homepage = "https://github.com/haskell/text";
              description = "An efficient packed Unicode text type";
              license = stdenv.lib.licenses.bsd2;
              doHaddock = false;
            }
          )
          { inherit QuickCheck random; };
        th-abstraction = callPackage
          (
            { mkDerivation, base, containers, ghc-prim, stdenv
            , template-haskell
            }:
            mkDerivation {
              pname = "th-abstraction";
              version = "0.2.6.0";
              sha256 = "e52e289a547d68f203d65f2e63ec2d87a3c613007d2fe873615c0969b981823c";
              revision = "1";
              editedCabalFile = "0k4s4nbg9jlgaza38842jnzs8s01ig85fzmjgd10k9hl02gc3r44";
              libraryHaskellDepends = [
                base containers ghc-prim template-haskell
              ];
              testHaskellDepends = [ base containers template-haskell ];
              homepage = "https://github.com/glguy/th-abstraction";
              description = "Nicer interface for reified information about data types";
              license = stdenv.lib.licenses.isc;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        transformers = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "transformers";
              version = "0.5.5.0";
              sha256 = "b11eb8827cfd48a801516adec27e2de4091f424386e4c99846c587fc108b19a5";
              libraryHaskellDepends = [ base ];
              description = "Concrete functor and monad transformers";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        hashable = callPackage
          (
            { mkDerivation, base, bytestring, criterion, deepseq, ghc-prim
            , HUnit, integer-gmp, QuickCheck, random, siphash, stdenv
            , test-framework, test-framework-hunit, test-framework-quickcheck2
            , text, unix
            }:
            mkDerivation {
              pname = "hashable";
              version = "1.2.7.0";
              sha256 = "ecb5efc0586023f5a0dc861100621c1dbb4cbb2f0516829a16ebac39f0432abf";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [
                base bytestring deepseq ghc-prim integer-gmp text
              ];
              testHaskellDepends = [
                base bytestring ghc-prim HUnit QuickCheck random test-framework
                test-framework-hunit test-framework-quickcheck2 text unix
              ];
              benchmarkHaskellDepends = [
                base bytestring criterion ghc-prim integer-gmp siphash text
              ];
              homepage = "http://github.com/tibbe/hashable";
              description = "A class for types that can be converted to a hash value";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck random text; };
        transformers-compat = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "transformers-compat";
              version = "0.6.1.6";
              sha256 = "bddedda48764a57cdacf030d8c27b40c313a7b37fb8d95045ac1917b41a26135";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              homepage = "http://github.com/ekmett/transformers-compat/";
              description = "A small compatibility shim for the transformers library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit transformers; };
        primitive = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "primitive";
              version = "0.6.3.0";
              sha256 = "cddeff804e0f577f1be0179d5d145dfc170f8bfb66f663b9fba67104a45d9555";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              testHaskellDepends = [ base ghc-prim ];
              homepage = "https://github.com/haskell/primitive";
              description = "Primitive memory-related operations";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit transformers; };
        StateVar = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers }:
            mkDerivation {
              pname = "StateVar";
              version = "1.1.1.0";
              sha256 = "1a89cd2632c2fc384b4c71fdc12894cc1c3902badbf4771497437e4044274e80";
              libraryHaskellDepends = [ base stm transformers ];
              homepage = "https://github.com/haskell-opengl/StateVar";
              description = "State variables";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit stm transformers; };
        unordered-containers = callPackage
          (
            { mkDerivation, base, bytestring, ChasingBottoms, containers
            , criterion, deepseq, deepseq-generics, hashable, hashmap, HUnit
            , mtl, QuickCheck, random, stdenv, test-framework
            , test-framework-hunit, test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "unordered-containers";
              version = "0.2.9.0";
              sha256 = "6730cb5c4a3e953e2c199d6425be08fd088ff0089a3e140d63226c052e318250";
              libraryHaskellDepends = [ base deepseq hashable ];
              testHaskellDepends = [
                base ChasingBottoms containers hashable HUnit QuickCheck
                test-framework test-framework-hunit test-framework-quickcheck2
              ];
              benchmarkHaskellDepends = [
                base bytestring containers criterion deepseq deepseq-generics
                hashable hashmap mtl random
              ];
              homepage = "https://github.com/tibbe/unordered-containers";
              description = "Efficient hashing-based container types";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable QuickCheck random; };
        tagged = callPackage
          (
            { mkDerivation, base, deepseq, stdenv, template-haskell
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "tagged";
              version = "0.8.5";
              sha256 = "e47c51c955ed77b0fa36897f652df990aa0a8c4eb278efaddcd604be00fc8d99";
              revision = "2";
              editedCabalFile = "0r2knfcq0b4s652vlvlnfwxlc2mkc2ra9kl8bp4zdn1awmfy0ia5";
              libraryHaskellDepends = [
                base deepseq template-haskell transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/tagged";
              description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit transformers transformers-compat; };
        tf-random = callPackage
          (
            { mkDerivation, base, primitive, random, stdenv, time }:
            mkDerivation {
              pname = "tf-random";
              version = "0.5";
              sha256 = "2e30cec027b313c9e1794d326635d8fc5f79b6bf6e7580ab4b00186dadc88510";
              libraryHaskellDepends = [ base primitive random time ];
              description = "High-quality splittable pseudorandom number generator";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit primitive random; };
        contravariant = callPackage
          (
            { mkDerivation, base, StateVar, stdenv, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "contravariant";
              version = "1.4.1";
              sha256 = "c93d3d619fa378f3fdf92c53bb8b04b8f47963b88aba7cfa54b57656189ad0ed";
              revision = "1";
              editedCabalFile = "0qj5nymccrb9p0cd6hffsy90jidjng14g9yv95z8v6h4q84sbzvx";
              libraryHaskellDepends = [
                base StateVar transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/contravariant/";
              description = "Contravariant functors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit StateVar transformers transformers-compat; };
        distributive = callPackage
          (
            { mkDerivation, base, base-orphans, Cabal, cabal-doctest, doctest
            , generic-deriving, hspec, stdenv, tagged, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "distributive";
              version = "0.5.3";
              sha256 = "9173805b9c941bda1f37e5aeb68ae30f57a12df9b17bd2aa86db3b7d5236a678";
              revision = "5";
              editedCabalFile = "0hl43mbw87s5l7p1iqc7iwz5rnzdcmj6g33pmq6hv4s9fg96j8x7";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base base-orphans tagged transformers transformers-compat
              ];
              testHaskellDepends = [ base doctest generic-deriving hspec ];
              homepage = "http://github.com/ekmett/distributive/";
              description = "Distributive functors -- Dual to Traversable";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans cabal-doctest tagged transformers transformers-compat; };
        QuickCheck = callPackage
          (
            { mkDerivation, base, containers, deepseq, random, stdenv
            , template-haskell, tf-random, transformers
            }:
            mkDerivation {
              pname = "QuickCheck";
              version = "2.11.3";
              sha256 = "488c5652139da0bac8b3e7d76f11320ded298549e62db530938bfee9ca981876";
              libraryHaskellDepends = [
                base containers deepseq random template-haskell tf-random
                transformers
              ];
              testHaskellDepends = [ base ];
              homepage = "https://github.com/nick8325/quickcheck";
              description = "Automatic testing of Haskell programs";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit random tf-random transformers; };
        comonad = callPackage
          (
            { mkDerivation, base, Cabal, cabal-doctest, containers
            , contravariant, distributive, doctest, semigroups, stdenv, tagged
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "comonad";
              version = "5.0.3";
              sha256 = "a7f4584d634051123c547f0d10f88eaf23d99229dbd01dfdcd98cddd41e54df6";
              revision = "2";
              editedCabalFile = "07gfz719y6q3bfv8jbvak78dda9g1qy4phl18cxisiapqdz31rry";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base containers contravariant distributive semigroups tagged
                transformers transformers-compat
              ];
              testHaskellDepends = [ base doctest ];
              homepage = "http://github.com/ekmett/comonad/";
              description = "Comonads";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit cabal-doctest contravariant distributive semigroups tagged transformers transformers-compat; };
        bifunctors = callPackage
          (
            { mkDerivation, base, base-orphans, comonad, containers, hspec
            , hspec-discover, QuickCheck, semigroups, stdenv, tagged
            , template-haskell, th-abstraction, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "bifunctors";
              version = "5.5.2";
              sha256 = "332bb2ea19e77dac55282daff8046d89f69514ced5b987779d887e53b5d7cb11";
              revision = "2";
              editedCabalFile = "0glrvir6md8a1ncr6ah95a5mnn7n9v8yl85afvdx24i9z1nr1319";
              libraryHaskellDepends = [
                base base-orphans comonad containers semigroups tagged
                template-haskell th-abstraction transformers transformers-compat
              ];
              testHaskellDepends = [
                base hspec QuickCheck template-haskell transformers
                transformers-compat
              ];
              testToolDepends = [ hspec-discover ];
              homepage = "http://github.com/ekmett/bifunctors/";
              description = "Bifunctors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans comonad QuickCheck semigroups tagged th-abstraction transformers transformers-compat; };
        semigroupoids = callPackage
          (
            { mkDerivation, base, base-orphans, bifunctors, Cabal
            , cabal-doctest, comonad, containers, contravariant, distributive
            , doctest, hashable, semigroups, stdenv, tagged, template-haskell
            , transformers, transformers-compat, unordered-containers
            }:
            mkDerivation {
              pname = "semigroupoids";
              version = "5.2.2";
              sha256 = "e4def54834cda65ac1e74e6f12a435410e19c1348e820434a30c491c8937299e";
              revision = "3";
              editedCabalFile = "1k7iq54rkiqrx5kdcc6mc11agqqcnp1hgrw6c6rl3yjybz1vc5y4";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base base-orphans bifunctors comonad containers contravariant
                distributive hashable semigroups tagged template-haskell
                transformers transformers-compat unordered-containers
              ];
              testHaskellDepends = [ base doctest ];
              homepage = "http://github.com/ekmett/semigroupoids";
              description = "Semigroupoids: Category sans id";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans bifunctors cabal-doctest comonad contravariant distributive hashable semigroups tagged transformers transformers-compat unordered-containers; };
        checkers = callPackage
          (
            { mkDerivation, array, base, QuickCheck, random, semigroupoids
            , stdenv
            }:
            mkDerivation {
              pname = "checkers";
              version = "0.4.10";
              sha256 = "89f739106f528998cc83bc25ab1b3e483cd2ffb21ca120fcb8b2e5c43306711e";
              libraryHaskellDepends = [
                array base QuickCheck random semigroupoids
              ];
              homepage = "https://github.com/conal/checkers";
              description = "Check properties on standard classes and data structures";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck random semigroupoids; };
      };

      newResolver = compiler.override {
        overrides = overrideFunction;
      };

    in newResolver;
}
