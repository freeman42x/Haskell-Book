{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}



module Ch16 where

import           Test.QuickCheck
import           Test.QuickCheck.Function

{-# ANN module "HLint: ignore Functor law" #-}
{-# ANN module "HLint: ignore Use newtype instead of data" #-}


a = (+1) <$> read "[1]" :: [Int]
-- Prelude> a
-- [2]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
-- Prelude> b
-- Just ["Hi,lol","Hellolol"]

c = (*2) <$> (\x -> x - 2)
-- Prelude> c 1
-- -2

d = ((return '1' ++) . show) . (\x -> [x, 1..3])
-- Prelude> d 0
-- "1[0,1,2,3]"

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read . ("123"++) . show <$> ioi
    in (*3) <$> changed
-- Prelude> e
-- 3693



functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  fmap (g . f) x == (fmap g . fmap f $ x)



newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary



data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = applyArbitrary2 Pair



data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap :: (c -> b) -> Two a c -> Two a b
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = applyArbitrary2 Two



data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap :: (c -> d) -> Three a b c -> Three a b d
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = applyArbitrary3 Three



data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) =>  Arbitrary (Three' a b) where
  arbitrary = applyArbitrary3 Three'



data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = applyArbitrary4 Four



data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = applyArbitrary4 Four'



main :: IO ()
main = do
  putStrLn "Identity"
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorCompose :: Identity Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "Pair"
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose :: Pair Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "Two"
  quickCheck (functorIdentity :: Two Double Int -> Bool)
  quickCheck (functorCompose :: Two Double Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "Three"
  quickCheck (functorIdentity :: Three Double Double Int -> Bool)
  quickCheck (functorCompose :: Three Double Double Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "Three'"
  quickCheck (functorIdentity :: Three' Double Int -> Bool)
  quickCheck (functorCompose :: Three' Double Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "Four"
  quickCheck (functorIdentity :: Four Double Double Double Int -> Bool)
  quickCheck (functorCompose :: Four Double Double Double Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "Four'"
  quickCheck (functorIdentity :: Four' Double Int -> Bool)
  quickCheck (functorCompose :: Four' Double Int -> Fun Int Int -> Fun Int Int -> Bool)



data Possibly a =
  LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers $ f a



data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap :: (c -> b) -> Sum a c -> Sum a b
  fmap _ (First a)  = First a
  fmap f (Second b) = Second $ f b



data Quant a b =
  Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Finance, Desk a, Bloor b]



newtype K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap :: (c -> b) -> K a c -> K a b
  fmap _ (K a) = K a

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary



newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap :: (c -> b) -> Flip K a c -> Flip K a b
  fmap f (Flip (K b)) = Flip (K $ f b)

instance (Arbitrary a, Arbitrary b, Arbitrary (f b a)) => Arbitrary (Flip f a b) where
  arbitrary = Flip <$> arbitrary



data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap :: (c -> b) -> EvilGoateeConst a c -> EvilGoateeConst a b
  fmap f (GoatyConst b) = GoatyConst $ f b

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary







main2 :: IO()
main2 = do
  putStrLn "Quant"
  quickCheck (functorIdentity :: Quant Int Int -> Bool)
  quickCheck (functorCompose :: Quant Int Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "K"
  quickCheck (functorIdentity :: K Int Int -> Bool)
  quickCheck (functorCompose :: K Int Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "Flip"
  quickCheck (functorIdentity :: Flip K Int Int -> Bool)
  quickCheck (functorCompose :: Flip K Int Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "EvilGoateeConst"
  quickCheck (functorIdentity :: EvilGoateeConst Int Int -> Bool)
  quickCheck (functorCompose :: EvilGoateeConst Int Int -> Fun Int Int -> Fun Int Int -> Bool)
