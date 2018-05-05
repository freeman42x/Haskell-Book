{-# LANGUAGE InstanceSigs #-}

module Ch17 where

import           Control.Applicative
import           Data.List
import           Data.Monoid

{-# ANN module "HLint: Move brackets to avoid $" #-}

-- Make the following expressions typecheck∷

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])



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
flatMap f as = undefined

-- instance Applicative List where
--   pure = undefined
--   (<*>) = undefined
