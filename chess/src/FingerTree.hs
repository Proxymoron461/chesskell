module FingerTree where

import qualified GHC.TypeLits as TL
import FirstClassFunctions
import Data.Type.Nat hiding (SNat(..))

data FingerTree (a :: Type) where
    Empty :: FingerTree a
    Single :: a -> FingerTree a
    Deep :: Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a

data Node a = Node2 a a | Node3 a a a
data Digit a = One a | Two a a | Three a a a | Four a a a a

type family (:<) (x :: a) (n :: FingerTree a) :: FingerTree a where
    x :< Empty                            = Single x
    x :< Single y                         = Deep (One x) Empty (One y)
    x :< Deep (One y)        middle right = Deep (Two x y) middle right
    x :< Deep (Two y z)      middle right = Deep (Three x y z) middle right
    x :< Deep (Three y z w)  middle right = Deep (Four x y z w) middle right
    x :< Deep (Four y z w a) Empty (One b) = Deep (Four x y z w) Empty (Two a b)
    x :< Deep (Four y z w a) Empty (Two b c) = Deep (Four x y z w) Empty (Three a b c)
    x :< Deep (Four y z w a) Empty (Three b c d) = Deep (Four x y z w) Empty (Four a b c d)
    x :< Deep (Four y z w a) middle right = Deep (Two x y) (Node3 z w a :< middle) right
infixr 5 :<

data (:<:) :: a -> FingerTree a -> Exp (FingerTree a)
type instance Eval (x :<: tree) = x :< tree

type family (:>) (n :: FingerTree a) (x :: a) :: FingerTree a where
    Empty                           :> x = Single x
    Single y                        :> x = Deep (One y) Empty (One x)
    Deep left middle (One y)        :> x = Deep left middle (Two y x)
    Deep left middle (Two y z)      :> x = Deep left middle (Three y z x)
    Deep left middle (Three y z w)  :> x = Deep left middle (Four y z w x)
    Deep (One b) Empty (Four y z w a) :> x = Deep (Two b y) Empty (Four z w a x)
    Deep (Two b c) Empty (Four y z w a) :> x = Deep (Three b c y) Empty (Four z w a x)
    Deep (Three b c d) Empty (Four y z w a) :> x = Deep (Four b c d y) Empty (Four z w a x)
    Deep left middle (Four y z w a) :> x = Deep left (middle :> Node3 y z w) (Two a x)
infixl 5 :>

data (:>:) :: FingerTree a -> a -> Exp (FingerTree a)
type instance Eval (tree :>: a) = tree :> a

type family Head (x :: FingerTree a) :: a where
    Head Empty = TL.TypeError (TL.Text "Attempting to get Head of an empty FingerTree!")
    Head (Single x)                = x
    Head (Deep (One x)        _ _) = x
    Head (Deep (Two x _)      _ _) = x
    Head (Deep (Three x _ _)  _ _) = x
    Head (Deep (Four x _ _ _) _ _) = x

type family Tail' (x :: FingerTree a) :: FingerTree a where
    Tail' Empty = TL.TypeError (TL.Text "Attempting to get Tail of an empty FingerTree!")
    Tail' (Single x) = Empty
    Tail' (Deep (Two x y) middle right) = Deep (One y) middle right
    Tail' (Deep (Three x y z) middle right) = Deep (Two y z) middle right
    Tail' (Deep (Four x y z w) middle right) = Deep (Three y z w) middle right
    Tail' (Deep (One x) middle right) = Eval (Foldr (Flip (Foldr (:<:))) (Eval (Foldr (:<:) Empty right)) middle)

-- Node instance for Map
type instance Eval (Map f (Node2 x y))    = Node2 (Eval (f x)) (Eval (f y))
type instance Eval (Map f (Node3 x y z))  = Node3 (Eval (f x)) (Eval (f y)) (Eval (f z))

-- Digit instance for Map
type instance Eval (Map f (One x))        = One (Eval (f x))
type instance Eval (Map f (Two x y))      = Two (Eval (f x)) (Eval (f y))
type instance Eval (Map f (Three x y z))  = Three (Eval (f x)) (Eval (f y)) (Eval (f z))
type instance Eval (Map f (Four x y z w)) = Four (Eval (f x)) (Eval (f y)) (Eval (f z)) (Eval (f w))

-- FingerTree instance for Map
type instance Eval (Map f Empty)          = Empty
type instance Eval (Map f (Single x))     = Single (Eval (f x))
type instance Eval (Map f (Deep left middle right))
    = Deep (Eval (Map f left)) (Eval (Map (Map f) middle)) (Eval (Map f right))


-- f :: (a -> b -> Exp b)
-- Digit instance for Foldr
type instance Eval (Foldr f z (One a))        = Eval (f a z)
type instance Eval (Foldr f z (Two a b))      = Eval (f a (Eval (f b z)))
type instance Eval (Foldr f z (Three a b c))  = Eval (f a (Eval (f b (Eval (f c z)))))
type instance Eval (Foldr f z (Four a b c d)) = Eval (f a (Eval (f b (Eval (f c (Eval (f d z)))))))

-- Node instance for Foldr
type instance Eval (Foldr f z (Node2 a b))   = Eval (f a (Eval (f b z)))
type instance Eval (Foldr f z (Node3 a b c)) = Eval (f a (Eval (f b (Eval (f c z)))))

-- FingerTree instance for Foldr
type instance Eval (Foldr f z Empty)                    = z
type instance Eval (Foldr f z (Single x))               = Eval (f x z)
type instance Eval (Foldr f z (Deep left middle right))
    = Eval (Foldr f (Eval (Foldr (Flip (Foldr f)) (Eval (Foldr f z right)) middle)) left)

-- FingerTree instance of (++)
type instance (leftTree ++ rightTree) = FingerTreeAppend leftTree rightTree

type family FingerTreeAppend (x :: FingerTree a) (y :: FingerTree a) :: FingerTree a where
    FingerTreeAppend Empty rightTree = rightTree
    FingerTreeAppend (Single x) Empty = Single x
    FingerTreeAppend (Single x) (Single y) = Deep (One x) Empty (One y)
    FingerTreeAppend (Single x) rightTree = x :< rightTree
    FingerTreeAppend (Deep leftL leftM leftR) Empty = (Deep leftL leftM leftR)
    FingerTreeAppend (Deep leftL leftM leftR) (Single x) = (Deep leftL leftM leftR) :> x
    FingerTreeAppend (Deep leftL leftM leftR) (Deep rightL rightM rightR)
        = Deep leftL (AddTree1Digit leftM (ToNode leftR rightL) rightM) rightR

type family ToNode (d1 :: Digit a) (d2 :: Digit a) :: Digit (Node a) where
    ToNode (One x) (One a) = One (Node2 x a)
    ToNode (One x) (Two a b) = One (Node3 x a b)
    ToNode (One x) (Three a b c) = Two (Node2 x a) (Node2 b c)
    ToNode (One x) (Four a b c d) = Two (Node3 x a b) (Node2 c d)
    ToNode (Two x y) (One a) = One (Node3 x y a)
    ToNode (Two x y) (Two a b) = Two (Node2 x y) (Node2 a b)
    ToNode (Two x y) (Three a b c) = Two (Node3 x y a) (Node2 b c)
    ToNode (Two x y) (Four a b c d) = Two (Node3 x y a) (Node3 b c d)
    ToNode (Three x y z) (One a) = Two (Node2 x y) (Node2 z a)
    ToNode (Three x y z) (Two a b) = Two (Node3 x y z) (Node2 a b)
    ToNode (Three x y z) (Three a b c) = Two (Node3 x y z) (Node3 a b c)
    ToNode (Three x y z) (Four a b c d) = Three (Node3 x y z) (Node2 a b) (Node2 c d)
    ToNode (Four x y z w) (One a) = Two (Node3 x y z) (Node2 w a)
    ToNode (Four x y z w) (Two a b) = Two (Node3 x y z) (Node3 w a b)
    ToNode (Four x y z w) (Three a b c) = Three (Node3 x y z) (Node2 w a) (Node2 b c)
    ToNode (Four x y z w) (Four a b c d) = Three (Node3 x y z) (Node3 w a b) (Node2 c d)

-- TODO: More base cases!
type family AddDigitLeft (x :: Digit a) (y :: FingerTree a) :: FingerTree a where
    AddDigitLeft (One x)        tree  = x :< tree
    AddDigitLeft (Two x y)      tree  = x :< y :< tree
    AddDigitLeft (Three x y z)  tree  = x :< y :< z :< tree
    AddDigitLeft (Four x y z w) tree  = x :< y :< z :< w :< tree

-- TODO: More base cases!
type family AddDigitRight (x :: FingerTree a) (y :: Digit a) :: FingerTree a where
    AddDigitRight tree (One x)        = tree :> x
    AddDigitRight tree (Two x y)      = tree :> y :> x
    AddDigitRight tree (Three x y z)  = tree :> z :> y :> x
    AddDigitRight tree (Four x y z w) = tree :> w :> z :> y :> x

type family AddTree1Digit (t1 :: FingerTree a) (d1 :: Digit a) (t2 :: FingerTree a) :: FingerTree a where
    AddTree1Digit Empty dig rightTree = AddDigitLeft dig rightTree
    AddTree1Digit (Single x) dig rightTree = x :< AddDigitLeft dig rightTree
    AddTree1Digit (Deep leftL leftM leftR) dig Empty = AddDigitRight (Deep leftL leftM leftR) dig
    AddTree1Digit (Deep leftL leftM leftR) dig (Single y) = AddDigitRight (Deep leftL leftM leftR) dig :> y
    AddTree1Digit (Deep leftL leftM leftR) dig (Deep rightL rightM rightR)
        = Deep leftL (AddTree1Digit leftM (ToNode leftR rightL) rightM) rightR

-- TODO: FingerTree instance of (<*>)

-- FingerTree instance of Filter
type instance Eval (Filter p Empty) = Empty
type instance Eval (Filter p (Single x)) = If' (Eval (p x)) (ID (Single x)) (ID Empty)
type instance Eval (Filter p (Deep left middle right)) = Eval (Foldr (IfCons p) Empty (Deep left middle right))

data IfCons :: (a -> Exp Bool) -> a -> FingerTree a -> Exp (FingerTree a)
type instance Eval (IfCons p x xs) = If' (Eval (p x)) (x :<: xs) (ID xs)

data FromList :: [a] -> Exp (FingerTree a)
type instance Eval (FromList xs) = Eval (Foldr (:<:) Empty xs)

data ToList :: f a -> Exp [a]
type instance Eval (ToList x) = Eval (Foldr Cons '[] x)

data Cons :: a -> [a] -> Exp [a]
type instance Eval (Cons x xs) = (x ': xs)

-- FingerTree instance of TakeWhilePlus
type instance Eval (TakeWhilePlus p q Empty) = Empty
type instance Eval (TakeWhilePlus p q (Single x)) = If' (Eval (p x)) (ID (Single x)) (ID Empty)
type instance Eval (TakeWhilePlus p q (Deep left middle right))
    = Eval (If (Eval (p (Head (Deep left middle right))))
        ((Head (Deep left middle right)) :<: Eval (TakeWhilePlus p q (Tail' (Deep left middle right))))
        (If (Eval (q (Head (Deep left middle right))))
            (ID (Single (Head (Deep left middle right))))
            (ID Empty)))

-- FingerTree instance of TypeShow
type instance TypeShow Empty = ""
type instance TypeShow (Single x) = TypeShow x
type instance TypeShow (Deep left middle right) = TypeShow (Eval (ToList (Deep left middle right)))
