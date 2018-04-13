{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

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



data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
  fmap f (LiftItOut fa) = LiftItOut $ f <$> fa

instance Arbitrary (f a) => Arbitrary (LiftItOut f a) where
  arbitrary = LiftItOut <$> arbitrary



data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap :: (a -> b) -> Parappa f g a -> Parappa f g b
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
  arbitrary = applyArbitrary2 DaWrappa



data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap :: (c -> b) -> IgnoreOne f g a c -> IgnoreOne f g a b
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (f <$> gb)

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
  arbitrary = applyArbitrary2 IgnoringSomething



data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap :: (c -> b) -> Notorious g o a c -> Notorious g o a b
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => Arbitrary (Notorious g o a t) where
  arbitrary = applyArbitrary3 Notorious



data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized arbitrarySizedList

arbitrarySizedList :: Arbitrary a => Int -> Gen (List a)
arbitrarySizedList n
  | n == 0 = return Nil
  | n > 5 = arbitrarySizedList 5
  | otherwise = Cons <$> arbitrary <*> arbitrarySizedList (n - 1)



data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats mg1 mg2 mg3) = MoreGoats (f <$> mg1) (f <$> mg2) (f <$> mg3)

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = sized arbitrarySizedGoatLord

arbitrarySizedGoatLord :: Arbitrary a => Int -> Gen (GoatLord a)
arbitrarySizedGoatLord n
  | n == 0 = return NoGoat
  | n > 3 = arbitrarySizedGoatLord 3
  | otherwise = frequency [(1, return NoGoat),
                           (1, OneGoat <$> arbitrary),
                           (8, MoreGoats <$> arbitrarySizedGoatLord (n-1)
                                         <*> arbitrarySizedGoatLord (n-1)
                                         <*> arbitrarySizedGoatLord (n-1))]



data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap :: (a -> b) -> TalkToMe a -> TalkToMe b
  fmap  _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read $ f . g



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

  putStrLn "LiftItOut"
  quickCheck (functorIdentity :: LiftItOut [] Int -> Bool)
  quickCheck (functorCompose :: LiftItOut [] Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "Parappa"
  quickCheck (functorIdentity :: Parappa [] [] Int -> Bool)
  quickCheck (functorCompose :: Parappa [] [] Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "IgnoreOne"
  quickCheck (functorIdentity :: IgnoreOne [] [] Int Int -> Bool)
  quickCheck (functorCompose :: IgnoreOne [] [] Int Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "Notorious"
  quickCheck (functorIdentity :: Notorious [] Int Int Int -> Bool)
  quickCheck (functorCompose :: Notorious [] Int Int Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "List"
  quickCheck (functorIdentity :: List Int -> Bool)
  quickCheck (functorCompose :: List Int -> Fun Int Int -> Fun Int Int -> Bool)

  putStrLn "GoatLord"
  quickCheck (functorIdentity :: GoatLord Int -> Bool)
  quickCheck (functorCompose :: GoatLord Int -> Fun Int Int -> Fun Int Int -> Bool)
