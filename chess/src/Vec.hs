module Vec where

import qualified GHC.TypeLits as TL
import FirstClassFunctions
import Data.Type.Nat hiding (SNat(..))

-- FIXME: a -> Vec n -> Vec (n + 1) a causes issues. Why??
data Vec (n :: Nat) (a :: Type) where
    VEnd   :: Vec Z a
    (:->)  :: a -> Vec n a -> Vec (S n) a
infixr 4 :->

-- Helper type family, to avoid the (:-> VEnd) bit.
type family (:<>) (x :: a) (y :: a) :: Vec (S (S Z)) a where
    x :<> y = x :-> y :-> VEnd
infixr 5 :<>

data VecToList :: Vec n a -> Exp [a]
type instance Eval (VecToList (x :-> xs)) = x ': Eval (VecToList xs)
type instance Eval (VecToList VEnd)       = '[]

-- Membership checking for vectors
type family Elem (x :: a) (ys :: Vec n a) :: Bool where
    Elem x (y :-> rest) = Eval ((Eval (x :==: y)) :||: (ID (Elem x rest)))
    Elem x VEnd         = 'False

data AllVec :: (a -> Exp Bool) -> Vec n a -> Exp Bool
type instance Eval (AllVec p VEnd)       = True
type instance Eval (AllVec p (x :-> xs)) = Eval (Eval (p x) :&&: AllVec p xs)

data AnyVec :: (a -> Exp Bool) -> Vec n a -> Exp Bool
type instance Eval (AnyVec p VEnd)       = False
type instance Eval (AnyVec p (x :-> xs)) = Eval (Eval (p x) :||: AnyVec p xs)

data VFilterCount :: (a -> Exp Bool) -> Vec n a -> Exp Nat
type instance Eval (VFilterCount p xs) = Eval (FilterCount p (Eval (VecToList xs)))

-- Vector instance for Map
type instance Eval (Map f VEnd)       = VEnd
type instance Eval (Map f (x :-> xs)) = Eval (f x) :-> Eval (Map f xs)

-- Vector instance for Length
type instance Eval (Length VEnd)       = Nat0
type instance Eval (Length (x :-> xs)) = Nat1 + Eval (Length xs)

-- Vector instance for Foldr
type instance Eval (Foldr f z VEnd)       = z
type instance Eval (Foldr f z (x :-> xs)) = Eval (f x (Eval (Foldr f z xs)))

type instance Eval (Find f VEnd)       = Nothing
type instance Eval (Find f (x :-> xs)) = Eval (If (Eval (f x)) (ID (Just x)) (Find f xs))

-- When using Maybes, this returns another maybe!
-- :kind! Eval (VecAt TestBoard Z) :: Maybe Row
-- data Bind :: (a -> Exp (f b)) -> f a -> Exp (f b)
data VecAt :: Vec n a -> Nat -> Exp (Maybe a)
type instance Eval (VecAt VEnd _)           = Nothing
type instance Eval (VecAt (x :-> xs) Z)     = Just x
type instance Eval (VecAt (x :-> xs) (S n)) = Eval (VecAt xs n)

-- :kind! Eval ((A :-> B :<> C) !! (S (S Z))) = 'Just C
data (!!) :: Vec n a -> Nat -> Exp (Maybe a)
type instance Eval (vec !! nat) = Eval (VecAt vec nat)

type family VAUgly (vec :: Vec Eight a) (n :: Nat) :: a where
    VAUgly (a :-> xs) Nat0 = a
    VAUgly (a :-> b :-> xs) Nat1 = b
    VAUgly (a :-> b :-> c :-> xs) Nat2 = c
    VAUgly (a :-> b :-> c :-> d :-> xs) Nat3 = d
    VAUgly (a :-> b :-> c :-> d :-> e :-> xs) Nat4 = e
    VAUgly (a :-> b :-> c :-> d :-> e :-> f :-> xs) Nat5 = f
    VAUgly (a :-> b :-> c :-> d :-> e :-> f :-> g :-> xs) Nat6 = g
    VAUgly (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> xs) Nat7 = h

data PutAt :: a -> Nat -> Vec n a -> Exp (Vec n a)
type instance Eval (PutAt x Z (y :-> ys))     = x :-> ys
type instance Eval (PutAt x (S n) (y :-> ys)) = y :-> Eval (PutAt x n ys)