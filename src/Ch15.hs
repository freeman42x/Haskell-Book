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
  arbitrary = Identity <$> arbitrary



main2 :: IO ()
main2 = do
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (semigroupAssoc :: Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool)
