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

-- :kind! Eval ((A :-> B :<> C) !! (S (S Z))) = 'Just C
data (!!) :: Vec Eight a -> Nat -> Exp (Maybe a)
type instance Eval (vec !! nat) = Just $ VecAt vec nat

type family VecAt (vec :: Vec Eight a) (n :: Nat) :: a where
    VecAt (a :-> _) Nat0 = a
    VecAt (a :-> b :-> _) Nat1 = b
    VecAt (a :-> b :-> c :-> _) Nat2 = c
    VecAt (a :-> b :-> c :-> d :-> _) Nat3 = d
    VecAt (a :-> b :-> c :-> d :-> e :-> _) Nat4 = e
    VecAt (a :-> b :-> c :-> d :-> e :-> f :-> _) Nat5 = f
    VecAt (a :-> b :-> c :-> d :-> e :-> f :-> g :-> _) Nat6 = g
    VecAt (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> _) Nat7 = h

data PutAt :: a -> Nat -> Vec n a -> Exp (Vec n a)
type instance Eval (PutAt x Z (y :-> ys))     = x :-> ys
type instance Eval (PutAt x (S n) (y :-> ys)) = y :-> Eval (PutAt x n ys)