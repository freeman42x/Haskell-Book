{-# LANGUAGE InstanceSigs #-}

module Ch15 where

import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty :: Optional a
  mempty = Nada

  mappend :: Optional a -> Optional a -> Optional a
  mappend Nada Nada = Nada
  mappend Nada (Only b) = Only b
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only b) = Only $ mappend a b



type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbin' e adv noun adj =
  e
  <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbinBetter' e adv noun adj = mconcat
  [e, "! he said ", adv, " as he jumped into his car "
  , noun, " and drove off with his ", adj, " wife."]



monoidAssoc :: (Eq m, Monoid m, Semigroup m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a



newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) :: First' a -> First' a -> First' a
  (<>) (First' Nada) (First' Nada) = First' Nada
  (<>) (First' Nada) b = b
  (<>) a _ = a

instance Monoid (First' a) where
  mempty :: First' a
  mempty = First' Nada

  mappend :: First' a -> First' a -> First' a
  mappend = (<>)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [(1, return (First' Nada)),
                         (4, First' . Only <$> arbitrary)]

type FirstMappend =
  First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

main1 :: IO ()
main1 = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)



semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial



newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity $ a <> a'

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary :: Gen (Identity a)
  arbitrary = Identity <$> arbitrary



data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = applyArbitrary2 Two



data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = applyArbitrary3 Three



data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
  => Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
  arbitrary = applyArbitrary4 Four



newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary



newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary



data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst a <> Fst b = Fst b
  Fst a <> Snd b = Snd b
  Snd a <> Fst b = Snd a
  Snd a <> Snd b = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

main2 :: IO ()
main2 = do
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (semigroupAssoc :: Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool)
  quickCheck (semigroupAssoc :: Two String String -> Two String String -> Two String String -> Bool)
  quickCheck (semigroupAssoc :: Three String String String
                                -> Three String String String
                                -> Three String String String
                                -> Bool)
  quickCheck (semigroupAssoc :: Four String String String String
                                -> Four String String String String
                                -> Four String String String String
                                -> Bool)
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: Or String String -> Or String String -> Or String String -> Bool)
