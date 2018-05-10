{-# LANGUAGE InstanceSigs #-}

module Ch15 where

import           Data.Monoid     hiding ((<>))
import           Data.Semigroup
import           Test.QuickCheck hiding (Failure, Success)

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty :: Optional a
  mempty = Nada

  mappend :: Optional a -> Optional a -> Optional a
  mappend Nada Nada         = Nada
  mappend Nada (Only b)     = Only b
  mappend (Only a) Nada     = Only a
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
  (<>) (First' Nada) b             = b
  (<>) a _                         = a

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

instance Monoid Trivial where
  mappend = (<>)
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial



newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity $ a <> a'

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mappend = (<>)
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary :: Gen (Identity a)
  arbitrary = Identity <$> arbitrary



data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mappend = (<>)
  mempty = Two mempty mempty

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

instance Monoid BoolConj where
  mappend = (<>)
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary



newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mappend = (<>)
  mempty = BoolDisj False

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



newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
   Combine f <> Combine f' = Combine $ f <> f'

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mappend = (<>)
  mempty = Combine mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary



newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
  Comp f <> Comp f' = Comp $ f . f'

instance Monoid (Comp a) where
  mappend = (<>)
  mempty = Comp id



main2 :: IO ()
main2 = do
  putStrLn "Trivial"
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  putStrLn "Identity"
  quickCheck (semigroupAssoc :: Identity [Int]
                                -> Identity [Int]
                                -> Identity [Int]
                                -> Bool)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  putStrLn "Two"
  quickCheck (semigroupAssoc :: Two String String
                                -> Two String String
                                -> Two String String
                                -> Bool)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

  putStrLn "Three"
  quickCheck (semigroupAssoc :: Three String String String
                                -> Three String String String
                                -> Three String String String
                                -> Bool)

  putStrLn "Four"
  quickCheck (semigroupAssoc :: Four String String String String
                                -> Four String String String String
                                -> Four String String String String
                                -> Bool)

  putStrLn "BoolConj"
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  putStrLn "BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  putStrLn "Or"
  quickCheck (semigroupAssoc :: Or String String
                                -> Or String String
                                -> Or String String
                                -> Bool)


data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure a <> Failure b = Failure $ a <> b
  Failure a <> Success b = Success b
  Success a <> _ = Success a

main3 = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2



newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem f' = Mem $ \s -> (fst (f s) <> fst (f' s), snd $ f (snd $ f' s))

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mappend = (<>)
  mempty = Mem $ \s -> (mempty, s)

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

main4 = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print rmleft
  print rmright
  print (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

-- Prelude> main
-- ("hi",1)
-- ("hi",1)
-- ("",0)
-- True
-- True
