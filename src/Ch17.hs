{-# LANGUAGE InstanceSigs #-}

module Ch17 where

import           Control.Applicative
import           Data.List
import           Data.Monoid
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Checkers  (EqProp, eq, quickBatch, (=-=))
import           Test.QuickCheck.Classes   (applicative)
import           Test.QuickCheck.Gen       (Gen, elements, frequency)

-- Make the following expressions typecheck:

added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])



y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z



x2 :: Maybe Int
x2 = elemIndex 3 [1, 2, 3, 4, 5]

y2 :: Maybe Int
y2 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x2 y2



xs = [1, 2, 3]
ys = [4, 5, 6]

x3 :: Maybe Integer
x3 = lookup 3 $ zip xs ys

y3 :: Maybe Integer
y3 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> liftA2 (,) x3 y3



newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (<*>) (Identity f) (Identity a) = Identity $ f a



newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap :: (c -> b) -> Constant a c -> Constant a b
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure :: b -> Constant a b
  pure b = Constant mempty
  (<*>) :: Constant a (c -> b) -> Constant a c -> Constant a b
  (<*>) (Constant a) (Constant b) = Constant $ a <> b



fixUp1 = const <$> Just "Hello" <*> Just "World"

fixUp2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]



data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f (Cons a la) = Cons (f a) (fmap f la)
  fmap _ _           = Nil

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure a = Cons a Nil

  (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fa <*> xs = append (fmap f xs) (fa <*> xs)



take' :: Int -> List a -> List a
take' 0 _           = Nil
take' n Nil         = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure :: a -> ZipList' a
  pure x = ZipList' (Cons x Nil)

  (<*>) :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' (Cons f Nil) <*> ZipList' (Cons x xs)  = ZipList' $ Cons (f x) (pure f <*> xs)
  ZipList' (Cons f fs)  <*> ZipList' (Cons x Nil) = ZipList' $ Cons (f x) (fs <*> pure x)
  ZipList' (Cons f fs)  <*> ZipList' (Cons x xs)  = ZipList' $ Cons (f x) (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [(3, return $ Cons h t),
             (1, return Nil)]

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary



data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)


instance Functor (Validation e) where
  fmap :: (a -> b) -> Validation e a -> Validation e b
  fmap f (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure :: a -> Validation e a
  pure = Success

  (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
  (Failure a) <*> (Failure b) = Failure $ a <> b
  (Failure a) <*> _ = Failure a
  _ <*> (Failure b) = Failure b
  (Success f) <*> (Success b) = Success $ f b

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure e, Success a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq



main :: IO()
main = do
  putStrLn "ZipList'"
  quickBatch (applicative $ ZipList' (Cons (undefined :: (Bool, Bool, Bool)) Nil))
  putStrLn "Validation"
  quickBatch (applicative (undefined :: Validation String (String, String, String)))
