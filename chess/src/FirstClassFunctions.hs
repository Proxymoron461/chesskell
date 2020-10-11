module FirstClassFunctions where

import GHC.TypeLits

-----------------------------------------------------------------------------------------------

-- A helpful kind synonym!
type Type = *

-- Defunctionalisation helpers! (thanks to https://github.com/Lysxia/first-class-families)
type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

-- Custom Nat class, to allow pattern matching on Nat > 2
data MyNat where
    Z :: MyNat
    S :: MyNat -> MyNat

data IsZero :: MyNat -> Exp Bool
type instance Eval (IsZero Z)     = True
type instance Eval (IsZero (S n)) = False

data Plus :: Nat -> Nat -> Exp Nat
type instance Eval (Plus x y) = x + y

data MyNatToNat :: MyNat -> Exp Nat
type instance Eval (MyNatToNat Z)     = 0
type instance Eval (MyNatToNat (S n)) = 1 + (Eval (MyNatToNat n))

data NatToMyNat :: Nat -> Exp MyNat
type instance Eval (NatToMyNat n) = NatToMyNatNonFCF n

type family NatToMyNatNonFCF (n :: Nat) :: MyNat where
    NatToMyNatNonFCF 0 = Z
    NatToMyNatNonFCF n = S (NatToMyNatNonFCF (n - 1))

-- ID function, for wrapping data in Exp
data ID :: a -> Exp a
type instance Eval (ID x) = x

-- :kind! Eval ((Not . IsZero) (S Z)) = True
-- :kind! Eval ((Not . IsZero) Z) = False
data (.) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((.) g f x) = Eval (g (Eval (f x)))
infixr 6 .

data Flip :: (a -> b -> Exp c) -> b -> a -> Exp c
type instance Eval (Flip f b a) = Eval (f a b)

data Curry :: (a -> b -> Exp c) -> (a, b) -> Exp c
type instance Eval (Curry f '(a, b)) = Eval (f a b)

-- Wrapping up a function, so that you can curry it at multiple layers!
data CurryWrap :: (a -> b) -> a -> Exp b
type instance Eval (CurryWrap f a) = f a
data CW :: (a -> b) -> a -> Exp b
type instance Eval (CW f a) = Eval (CurryWrap f a)
data CW2 :: (a -> b -> c) -> a -> b -> Exp c
type instance Eval (CW2 f a b) = f a b

-- Curry-able add function!
data Add :: Nat -> Nat -> Exp Nat
type instance Eval (Add x y)    = x + y

-- Type-level functors! (Almost)
data Map :: (a -> Exp b) -> f a -> Exp (f b)
-- Maybe instance
type instance Eval (Map f Nothing)  = Nothing
type instance Eval (Map f (Just x)) = Just (Eval (f x))
-- List instance
type instance Eval (Map f '[])       = '[]
type instance Eval (Map f (x ': xs)) = Eval (f x) ': Eval (Map f xs)

data (<$>) :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (f <$> x) = Eval (Map f x)

-- Type-level applicative functors! (Almost)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- :kind! Eval (Map (Add 1) (Just 1)) = 'Just 2
-- :kind! Eval (Apply (Eval (Map (CW Add) (Just 1))) (Just 5)) = 'Just 6
-- :kind! Eval ((Eval ((CW Plus) <$> [2,1,0])) <*> [1,2,3]) = '[3,4,5,2,3,4,1,2,3]

data Apply :: f (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Apply _ Nothing)         = Nothing
type instance Eval (Apply (Just f) (Just x)) = Just (Eval (f x))
type instance Eval (Apply '[] _) = '[]
type instance Eval (Apply (f ': fs) xs) = Eval (Eval (f <$> xs) ++ Eval (Apply fs xs))

data (<*>) :: f (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (f <*> x) = Eval (Apply f x)

data Bind :: (a -> Exp (f b)) -> f a -> Exp (f b)
type instance Eval (Bind f Nothing)  = Nothing
type instance Eval (Bind f (Just x)) = Eval (f x)

data (>>=) :: f a -> (a -> Exp (f b)) -> Exp (f b)
type instance Eval (x >>= f) = Eval (Bind f x)

data Join :: m (m a) -> Exp (m a)
type instance Eval (Join Nothing)  = Nothing
type instance Eval (Join (Just x)) = x

-- Some new thing - surely it already exists
data Flatten :: f (a -> Exp (f b)) -> f a -> Exp (f b)
type instance Eval (Flatten f x) = Eval (Join (Eval (Apply f x)))

-- This delays the evaluation of the type error!
-- (Thanks https://blog.poisson.chat/posts/2018-08-06-one-type-family.html#fnref4)
data TE' :: ErrorMessage -> Exp a
type instance Eval (TE' msg) = TypeError msg

-- A quick way of checking if two types are equal!
-- TODO: Test this to make sure it all works??
data IsTypeEqual :: a -> b -> Exp Bool
type instance Eval (IsTypeEqual a b) = IsTypeEqualNonFCF a b
data (:==:) :: a -> b -> Exp Bool
type instance Eval (a :==: b) = Eval (IsTypeEqual a b)

-- Function for checking list equality by elements
data (:=:=:) :: [a] -> [b] -> Exp Bool
type instance Eval (xs :=:=: ys) = Eval (Eval (xs :<= ys) :&&: (ys :<= xs))

-- Function for checking if a list is a subset of another list
data (:<=) :: [a] -> [b] -> Exp Bool
type instance Eval (xs :<= ys) = Eval (All ((Flip In) ys) xs)

data Reverse :: [a] -> Exp [a]
type instance Eval (Reverse '[]) = '[]
type instance Eval (Reverse (x ': xs)) = Eval (Eval (Reverse xs) ++ '[x])

-- TODO: Maybe make y have kind a as well?
type family IsTypeEqualNonFCF (x :: a) (y :: b) :: Bool where
    IsTypeEqualNonFCF x x = 'True
    IsTypeEqualNonFCF x y = 'False

-- A type-level show function!
data TypeShow :: a -> Exp Symbol

-- :kind! Eval (If (Eval (IsJust (Eval (GetPieceAt TestBoard (At "a" 1))))) (ID "yes") (ID "no")) = "yes"
-- :kind! Eval (If (Eval (IsJust (Eval (GetPieceAt TestBoard (At "a" 2))))) (ID "yes") (ID "no")) = "no"
data If :: Bool -> Exp b -> Exp b -> Exp b
type instance Eval (If 'True thenDo elseDo) = Eval thenDo
type instance Eval (If 'False  thenDo elseDo) = Eval elseDo

-- :kind! Eval (MaybeIf IsValidColumn (Just "a")) = 'True
-- :kind! Eval (MaybeIf IsValidColumn (Just "z")) = 'False
data MaybeIf :: (a -> Exp Bool) -> Maybe a -> Exp Bool
type instance Eval (MaybeIf p Nothing)  = False
type instance Eval (MaybeIf p (Just x)) = Eval (p x)

data IsJust :: Maybe a -> Exp Bool
type instance Eval (IsJust (Just _)) = True
type instance Eval (IsJust Nothing)  = False

data IsNothing :: Maybe a -> Exp Bool
type instance Eval (IsNothing x) = Eval ((Not . IsJust) x)

data FromJust :: Maybe a -> Exp a
type instance Eval (FromJust (Just x)) = x

data ToJust :: a -> Exp (Maybe a)
type instance Eval (ToJust x) = Just x

data FromMaybe :: b -> (a -> Exp b) -> Maybe a -> Exp b
type instance Eval (FromMaybe b f Nothing)  = b
type instance Eval (FromMaybe b f (Just x)) = Eval (f x)

data Const :: a -> b -> Exp a
type instance Eval (Const a _) = a


-- :kind! Eval (Holds [IsJust, MaybeIf IsZero] (Just Z)) = 'True
-- :kind! Eval (Holds [IsJust, MaybeIf IsZero] (Just (S Z))) = 'False
-- Applies a list of predicates to a value, and only passes if all the predicates are true.
data Holds :: [a -> Exp Bool] -> a -> Exp Bool
type instance Eval (Holds '[] x)       = True
type instance Eval (Holds (f ': fs) x) = Eval ((Eval (f x)) :&&: (Holds fs x))

data In :: a -> [a] -> Exp Bool
type instance Eval (In x ys) = Eval (Any ((:==:) x) ys)

-- :kind! Eval (Or True (TE' (Text "eeeeh")))
-- A lazy version of Or, which only evaluates its' second param if the first fails.
data LazyOr :: Bool -> Exp Bool -> Exp Bool
type instance Eval (LazyOr True  _) = True
type instance Eval (LazyOr False x) = Eval x

data (:||:) :: Bool -> Exp Bool -> Exp Bool
type instance Eval (x :||: y) = Eval (LazyOr x y)

data LazyAnd :: Bool -> Exp Bool -> Exp Bool
type instance Eval (LazyAnd False _) = False
type instance Eval (LazyAnd True x)  = Eval x

data (:&&:) :: Bool -> Exp Bool -> Exp Bool
type instance Eval (x :&&: y) = Eval (LazyAnd x y)

data Not :: Bool -> Exp Bool
type instance Eval (Not True)  = False
type instance Eval (Not False) = True

data Any :: (a -> Exp Bool) -> [a] -> Exp Bool
type instance Eval (Any p '[])       = False
type instance Eval (Any p (x ': xs)) = Eval (Eval (p x) :||: Any p xs)

data All :: (a -> Exp Bool) -> [a] -> Exp Bool
type instance Eval (All p '[])       = True
type instance Eval (All p (x ': xs)) = Eval (Eval (p x) :&&: All p xs)

data Length :: t a -> Exp Nat
type instance Eval (Length '[])        = 0
type instance Eval (Length (x ': xs))  = 1 + Eval (Length xs)

data Tail :: [a] -> Exp [a]
type instance Eval (Tail '[])       = '[]
type instance Eval (Tail (x ': xs)) = xs

-- TODO: FilterMap instance for vectors??
data FilterMap :: (a -> Exp Bool) -> (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (FilterMap p f (x ': xs)) = Eval (If (Eval (p x)) (ID (Eval (f x) ': Eval (FilterMap p f xs))) (FilterMap p f xs))
type instance Eval (FilterMap p f '[])       = '[]

data MapFilter :: (a -> Exp b) -> (b -> Exp Bool) -> [a] -> Exp [b]
type instance Eval (MapFilter f p '[])       = '[]
type instance Eval (MapFilter f p (x ': xs)) = Eval (MapFilterHelper (Eval (f x)) f p xs)

data MapFilterHelper :: b -> (a -> Exp b) -> (b -> Exp Bool) -> [a] -> Exp [b]
type instance Eval (MapFilterHelper x f p ys) = Eval (If (Eval (p x)) (ID (x ': (Eval (MapFilter f p ys)))) (MapFilter f p ys))

-- TODO: Kind-polymorphic Filter instances??
data Filter :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (Filter p '[]) = '[]
type instance Eval (Filter p (x ': xs)) = Eval (If (Eval (p x)) (ID (x ': Eval (Filter p xs))) (Filter p xs))

data FilterCount :: (a -> Exp Bool) -> [a] -> Exp Nat
type instance Eval (FilterCount p xs) = Eval (Length (Eval (Filter p xs)))

data Take :: Nat -> [a] -> Exp [a]
type instance Eval (Take n xs) = Eval (TakeMyNat (Eval (NatToMyNat n)) xs)

data TakeMyNat :: MyNat -> [a] -> Exp [a]
type instance Eval (TakeMyNat Z     _)         = '[]
type instance Eval (TakeMyNat (S n) '[])       = '[]
type instance Eval (TakeMyNat (S n) (x ': xs)) = x ': Eval (TakeMyNat n xs)

data TakeWhile :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (TakeWhile p xs) = Eval (TakeWhilePlus p (Const False) xs)

-- The regular TakeWhile, but on the first element that fails the test, it runs a second predicate.
-- Useful for empty spaces, and then checking the first piece you come across is of a different team.
data TakeWhilePlus :: (a -> Exp Bool) -> (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (TakeWhilePlus p q '[]) = '[]
type instance Eval (TakeWhilePlus p q (x ': xs)) = Eval (If (Eval (p x)) (ID (x ': Eval (TakeWhilePlus p q xs))) (If (Eval (q x)) (ID '[ x ]) (ID '[])))

data Zip :: [a] -> [b] -> Exp [(a, b)]
type instance Eval (Zip xs ys) = Eval (ZipWith xs ys (CW2 '(,)))

-- :kind! Eval (ZipWith '["a", "b"] '[1, 2] (CW2 At)) = '[ 'At "a" 1, 'At "b" 2]
data ZipWith :: [a] -> [b] -> (a -> b -> Exp c) -> Exp [c]
type instance Eval (ZipWith '[] _ _) = '[]
type instance Eval (ZipWith _ '[] _) = '[]
type instance Eval (ZipWith (x ': xs) (y ': ys) f) = Eval (f x y) ': Eval (ZipWith xs ys f)

data Foldr :: (a -> b -> Exp b) -> b -> f a -> Exp b
type instance Eval (Foldr f z '[])       = z
type instance Eval (Foldr f z (x ': xs)) = Eval (f x (Eval (Foldr f z xs)))

-- TODO: Make more type safe?? Currently 1 ++ 2 would compile without issues
data (++) :: a -> a -> Exp a
type instance Eval ('[] ++ ys) = ys
type instance Eval ((x ': xs) ++ ys) = x ': (Eval (xs ++ ys))
type instance Eval ((xs :: Symbol) ++ (ys :: Symbol)) = xs `AppendSymbol` ys

data Concat :: [[a]] -> Exp [a]
type instance Eval (Concat xs) = Eval (Foldr (++) '[] xs)

data Replicate :: Nat -> a -> Exp [a]
type instance Eval (Replicate n x) = Eval (ReplicateMyNat (Eval (NatToMyNat n)) x)

data ReplicateMyNat :: MyNat -> a -> Exp [a]
type instance Eval (ReplicateMyNat Z x)     = '[]
type instance Eval (ReplicateMyNat (S n) x) = x ': Eval (ReplicateMyNat n x)
