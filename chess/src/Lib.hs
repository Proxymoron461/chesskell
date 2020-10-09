module Lib where

import GHC.TypeLits

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- A helpful kind synonym!
type Type = *
-- A datatype for Proxy types!
data Proxy a = Proxy

-- Defunctionalisation helpers! (thanks to https://github.com/Lysxia/first-class-families)
type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

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
-- Vector instance
type instance Eval (Map f VEnd)       = VEnd
type instance Eval (Map f (x :-> xs)) = Eval (f x) :-> Eval (Map f xs)
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

-----------------------------------------------------------------------------------------------

-- FIXME: a -> Vec n -> Vec (n + 1) a causes issues. Why??
data Vec (n :: MyNat) (a :: Type) where
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

data AnyVec :: (a -> Exp Bool) -> Vec n a -> Exp Bool
type instance Eval (AnyVec p VEnd)       = False
type instance Eval (AnyVec p (x :-> xs)) = Eval (Eval (p x) :||: AnyVec p xs)

data All :: (a -> Exp Bool) -> [a] -> Exp Bool
type instance Eval (All p '[])       = True
type instance Eval (All p (x ': xs)) = Eval (Eval (p x) :&&: All p xs)

data AllVec :: (a -> Exp Bool) -> Vec n a -> Exp Bool
type instance Eval (AllVec p VEnd)       = True
type instance Eval (AllVec p (x :-> xs)) = Eval (Eval (p x) :&&: AllVec p xs)

data Length :: t a -> Exp Nat
type instance Eval (Length VEnd)       = 0
type instance Eval (Length (x :-> xs)) = 1 + Eval (Length xs)
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

-- :kind! Eval (ZipWith '["a", "b"] '[1, 2] (CW2 At)) = '[ 'At "a" 1, 'At "b" 2]
data ZipWith :: [a] -> [b] -> (a -> b -> Exp c) -> Exp [c]
type instance Eval (ZipWith '[] _ _) = '[]
type instance Eval (ZipWith _ '[] _) = '[]
type instance Eval (ZipWith (x ': xs) (y ': ys) f) = Eval (f x y) ': Eval (ZipWith xs ys f)

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr f z '[])       = z
type instance Eval (Foldr f z (x ': xs)) = Eval (f x (Eval (Foldr f z xs)))

data (++) :: [a] -> [a] -> Exp [a]
type instance Eval ('[] ++ ys) = ys
type instance Eval ((x ': xs) ++ ys) = x ': (Eval (xs ++ ys))

data Concat :: [[a]] -> Exp [a]
type instance Eval (Concat xs) = Eval (Foldr (++) '[] xs)

-- Type synonym for an 8x8 grid
type Eight = S (S (S (S (S (S (S (S Z)))))))
type Grid8x8 = Vec Eight (Vec Eight (Maybe Piece))

-- TODO: Dimensions of board in kind??
type Board = Grid8x8

data Piece where
    MkPiece :: Team -> PieceName -> PieceInfo -> Piece

data Team = Black | White

-- Make singleton types for each piece??
data PieceName = Pawn
               | Bishop
               | Knight
               | Rook
               | King
               | Queen

-- Holds the number of moves they've made, plus their current position.
-- While their position is implicit from where they are in the board, it's
-- helpful!
data PieceInfo where
    Info :: MyNat -> Position -> PieceInfo

data GetMoveCount :: PieceInfo -> Exp MyNat
type instance Eval (GetMoveCount (Info x _)) = x

data GetPosition :: PieceInfo -> Exp Position
type instance Eval (GetPosition (Info _ x)) = x

data PieceMoveCount :: Piece -> Exp MyNat
type instance Eval (PieceMoveCount (MkPiece _ _ info)) = Eval (GetMoveCount info)

data PiecePosition :: Piece -> Exp Position
type instance Eval (PiecePosition (MkPiece _ _ info)) = Eval (GetPosition info)

data PieceTeam :: Piece -> Exp Team
type instance Eval (PieceTeam (MkPiece team _ _)) = team

data PieceType :: Piece -> Exp PieceName
type instance Eval (PieceType (MkPiece _ name _)) = name

-- TODO: Type level char??
-- Goes column-row, e.g. At "a" 4 means first column from left, 4 up from the bottom, where Black is at the top
data Position where
    At :: Symbol -> Nat -> Position

type ValidColumns = "a" :-> "b" :-> "c" :-> "d" :-> "e" :-> "f" :-> "g" :<> "h"

type ValidRows = 1 :-> 2 :-> 3 :-> 4 :-> 5 :-> 6 :-> 7 :<> 8

data ValidColumn :: Symbol -> Exp (Maybe Symbol)
type instance Eval (ValidColumn x) = Eval (If (Elem x ValidColumns) (ID (Just x)) (ID Nothing))

data IsValidColumn :: Symbol -> Exp Bool
type instance Eval (IsValidColumn x) = Eval (IsJust (Eval (ValidColumn x)))

data IsValidRow :: Nat -> Exp Bool
type instance Eval (IsValidRow x) = Eval (If (Elem x ValidRows) (ID True) (ID False))

data IsValidPosition :: Position -> Exp Bool
type instance Eval (IsValidPosition (At col row)) = Eval ((Eval (IsValidColumn col)) :&&: (IsValidRow row))

-- Generates a range between two Nat values, non-inclusive of the first argument
data RangeBetween :: Nat -> Nat -> Exp [Nat]
type instance Eval (RangeBetween n m) = Eval (RangeBetweenHelper n m (CmpNat n m))

data RangeBetweenHelper :: Nat -> Nat -> Ordering -> Exp [Nat]
type instance Eval (RangeBetweenHelper n m LT) = (n + 1) ': (Eval (RangeBetweenHelper (n + 1) m (CmpNat (n + 1) m)))
type instance Eval (RangeBetweenHelper n m EQ) = '[]
type instance Eval (RangeBetweenHelper n m GT) = (n - 1) ': (Eval (RangeBetweenHelper (n - 1) m (CmpNat (n - 1) m)))

data RangeBetweenMyNat :: Nat -> Nat -> Exp [MyNat]
type instance Eval (RangeBetweenMyNat n m) = Eval (Map NatToMyNat (Eval (RangeBetween n m)))

-- Generates a range between two Char values, non-inclusive of the first argument
-- It will only go from lowercase "a" to lowercase "z"
data CharRangeBetween :: Symbol -> Symbol -> Exp [Symbol]
type instance Eval (CharRangeBetween a b) = Eval (CharRangeBetweenHelper a b (CmpSymbol a b))

data CharRangeBetweenHelper :: Symbol -> Symbol -> Ordering -> Exp [Symbol]
type instance Eval (CharRangeBetweenHelper a b LT) = Eval (TakeWhile (CharLessThan b) (Eval (Map FromJust (Eval (Filter IsJust (Eval (Map ((Flip (:+)) a) (Eval (RangeBetweenMyNat 0 8)))))))))
type instance Eval (CharRangeBetweenHelper a b EQ) = '[]
type instance Eval (CharRangeBetweenHelper a b GT) = Eval (TakeWhile (CharGreaterThan b) (Eval (Map FromJust (Eval (Filter IsJust (Eval (Map ((Flip (:-)) a) (Eval (RangeBetweenMyNat 0 8)))))))))

data CharLessThan :: Symbol -> Symbol -> Exp Bool
type instance Eval (CharLessThan b a) = Eval (IsLTEQ (CmpSymbol a b))

data CharGreaterThan :: Symbol -> Symbol -> Exp Bool
type instance Eval (CharGreaterThan b a) = Eval (IsGTEQ (CmpSymbol a b))

data IsLTEQ :: Ordering -> Exp Bool
type instance Eval (IsLTEQ LT) = True
type instance Eval (IsLTEQ EQ) = True
type instance Eval (IsLTEQ GT) = False

data IsGTEQ :: Ordering -> Exp Bool
type instance Eval (IsGTEQ LT) = False
type instance Eval (IsGTEQ EQ) = True
type instance Eval (IsGTEQ GT) = True

data GetAllBelow :: Position -> Exp [Position]
type instance Eval (GetAllBelow (At col row)) = Eval (Filter IsValidPosition (Eval (Map (CW (At col)) (Eval (RangeBetween row 0)))))

data GetNBelow :: Nat -> Position -> Exp [Position]
type instance Eval (GetNBelow n (At col row)) = Eval (Filter IsValidPosition (Eval (Map (CW (At col)) (Eval (RangeBetween row (Eval (SafeMinus row n)))))))

-- Takes in x and y, and performs x - y with a lower bound of 0
data SafeMinus :: Nat -> Nat -> Exp Nat
type instance Eval (SafeMinus x y) = Eval (SafeMinusHelper x y (CmpNat x y))

data SafeMinusHelper :: Nat -> Nat -> Ordering -> Exp Nat
type instance Eval (SafeMinusHelper x y LT) = 0
type instance Eval (SafeMinusHelper x y EQ) = 0
type instance Eval (SafeMinusHelper x y GT) = x - y

data GetAllAbove :: Position -> Exp [Position]
type instance Eval (GetAllAbove (At col row)) = Eval (Filter IsValidPosition (Eval (Map (CW (At col)) (Eval (RangeBetween row 8)))))

data GetNAbove :: Nat -> Position -> Exp [Position]
type instance Eval (GetNAbove n (At col row)) = Eval (Filter IsValidPosition (Eval (Map (CW (At col)) (Eval (RangeBetween row (row + n))))))

-- (:+) :: MyNat -> Symbol -> Exp (Maybe Symbol)
data GetNRight :: Nat -> Position -> Exp [Position]
type instance Eval (GetNRight n pos) = Eval (Filter IsValidPosition (Eval (GetNRightPositionsNoChecks n pos)))

-- TODO: Combine with GetNLeftMaybes to achieve DRY?
data GetNRightMaybes :: Nat -> Position -> Exp [Maybe Symbol]
type instance Eval (GetNRightMaybes n (At col row)) = Eval (Filter IsJust (Eval (Map ((Flip (:+)) col) (Eval (RangeBetweenMyNat 0 n)))))

-- TODO: Combine with GetNLeftPositionsNoChecks to achieve DRY?
data GetNRightPositionsNoChecks :: Nat -> Position -> Exp [Position]
type instance Eval (GetNRightPositionsNoChecks n (At col row)) = Eval (Map (((Flip (CW2 At)) row) . FromJust) (Eval (GetNRightMaybes n (At col row))))

data GetAllRight :: Position -> Exp [Position]
type instance Eval (GetAllRight pos) = Eval (GetNRight 8 pos)

data GetNLeft :: Nat -> Position -> Exp [Position]
type instance Eval (GetNLeft n pos) = Eval (Filter IsValidPosition (Eval (GetNLeftPositionsNoChecks n pos)))

-- Can reverse with RangeBetweenMyNat (n + 1) 1
data GetNLeftMaybes :: Nat -> Position -> Exp [Maybe Symbol]
type instance Eval (GetNLeftMaybes n (At col row)) = Eval (Filter IsJust (Eval (Map ((Flip (:-)) col) (Eval (RangeBetweenMyNat 0 n)))))

data GetNLeftPositionsNoChecks :: Nat -> Position -> Exp [Position]
type instance Eval (GetNLeftPositionsNoChecks n (At col row)) = Eval (Map (((Flip (CW2 At)) row) . FromJust) (Eval (GetNLeftMaybes n (At col row))))

data GetAllLeft :: Position -> Exp [Position]
type instance Eval (GetAllLeft pos) = Eval (GetNLeft 8 pos)

data GetAllDiagNW :: Position -> Exp [Position]
type instance Eval (GetAllDiagNW (At col row)) = Eval (ZipWith (Eval (CharRangeBetween col "h")) (Eval (RangeBetween row 8)) (CW2 At))

data GetAllDiagSW :: Position -> Exp [Position]
type instance Eval (GetAllDiagSW (At col row)) = Eval (ZipWith (Eval (CharRangeBetween col "h")) (Eval (RangeBetween row 1)) (CW2 At))

data GetAllDiagSE :: Position -> Exp [Position]
type instance Eval (GetAllDiagSE (At col row)) = Eval (ZipWith (Eval (CharRangeBetween col "a")) (Eval (RangeBetween row 1)) (CW2 At))

data GetAllDiagNE :: Position -> Exp [Position]
type instance Eval (GetAllDiagNE (At col row)) = Eval (ZipWith (Eval (CharRangeBetween col "a")) (Eval (RangeBetween row 8)) (CW2 At))

data GetAllKnightPositions :: Position -> Exp [Position]
type instance Eval (GetAllKnightPositions pos) = Eval (Filter IsValidPosition (Eval (Eval (GetKnightAboveBelow pos) ++ Eval (GetKnightLeftRight pos))))

data GetKnightAboveBelow :: Position -> Exp [Position]
type instance Eval (GetKnightAboveBelow (At col row)) = Eval (Eval (CW (CW2 At) <$> Eval (GetKnightColumns col 1)) <*> Eval (GetKnightRows row 2))

data GetKnightLeftRight :: Position -> Exp [Position]
type instance Eval (GetKnightLeftRight (At col row)) = Eval (Eval (CW (CW2 At) <$> Eval (GetKnightColumns col 2)) <*> Eval (GetKnightRows row 1))

data GetKnightColumns :: Symbol -> Nat -> Exp [Symbol]
type instance Eval (GetKnightColumns col n) = Eval (GetKnightColumnsMyNat col (Eval (NatToMyNat n)))

data GetKnightColumnsMyNat :: Symbol -> MyNat -> Exp [Symbol]
type instance Eval (GetKnightColumnsMyNat col n) = Eval (FilterMap (IsJust) (FromJust) '[ Eval (n :+ col), Eval (n :- col) ])

data GetKnightRows :: Nat -> Nat -> Exp [Nat]
type instance Eval (GetKnightRows row n) = Eval (If (n <=? row) (ID '[ row - n, row + n]) (ID '[ row + n ]))

-- :kind! Eval ((Eval ((CW Plus) <$> [2,1,0])) <*> [1,2,3])
-- NOTE: Uses Tail to remove the current position!
data GetAdjacent :: Position -> Exp [Position]
type instance Eval (GetAdjacent (At col row)) = Eval (Filter IsValidPosition (Eval (Tail (Eval ((Eval (CW (CW2 At) <$> (Eval (GetAdjacentColumns col)))) <*> '[row, row + 1, Eval (SafeMinus row 1)])))))

data GetAdjacentColumns :: Symbol -> Exp [Symbol]
type instance Eval (GetAdjacentColumns col) = col ': Eval (Map FromJust (Eval (Filter IsJust '[Eval ((S Z) :+ col), Eval ((S Z) :- col)])))

data IsOpposingTeam :: Piece -> Piece -> Exp Bool
type instance Eval (IsOpposingTeam (MkPiece White _ _) (MkPiece White _ _)) = False
type instance Eval (IsOpposingTeam (MkPiece Black _ _) (MkPiece Black _ _)) = False
type instance Eval (IsOpposingTeam (MkPiece White _ _) (MkPiece Black _ _)) = True
type instance Eval (IsOpposingTeam (MkPiece Black _ _) (MkPiece White _ _)) = True

data IsSameTeam :: Piece -> Piece -> Exp Bool
type instance Eval (IsSameTeam p1 p2) = Eval ((Not . (IsOpposingTeam p1)) p2)

data HasTeam :: Team -> Piece -> Exp Bool
type instance Eval (HasTeam White (MkPiece White _ _)) = True
type instance Eval (HasTeam Black (MkPiece Black _ _)) = True
type instance Eval (HasTeam White (MkPiece Black _ _)) = False
type instance Eval (HasTeam Black (MkPiece White _ _)) = False

-- Type families for getting all available squares in a straight line, with nothing in the way
data AllReachableFunc :: Team -> Board -> Position -> (Position -> Exp [Position]) -> Exp [Position]
type instance Eval (AllReachableFunc team board pos f) = Eval (TakeWhilePlus (Not . (IsPieceAt board)) ((MaybeIf (Not . (HasTeam team))) . (GetPieceAt board)) (Eval (f pos)))

data AllReachableLeft :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableLeft team board pos) = Eval (AllReachableFunc team board pos GetAllLeft)

data AllReachableRight :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableRight team board pos) = Eval (AllReachableFunc team board pos GetAllRight)

data AllReachableAbove :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableAbove team board pos) = Eval (AllReachableFunc team board pos GetAllAbove)

data AllReachableBelow :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableBelow team board pos) = Eval (AllReachableFunc team board pos GetAllBelow)

data AllReachableStraightLine :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableStraightLine team board pos) = Eval (Concat (Eval (Map (AllReachableFunc team board pos) '[ GetAllLeft, GetAllRight, GetAllAbove, GetAllBelow ])))

data AllReachableLineAndDiag :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableLineAndDiag team board pos) = Eval ((Eval (AllReachableStraightLine team board pos)) ++ (Eval (AllReachableDiag team board pos)))

-- Reachable square type families for all diagonal directions at once: helpful
-- for Bishops and Queens!
data AllReachableDiag :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableDiag team board pos) = Eval (Concat (Eval (Map (AllReachableFunc team board pos) '[ GetAllDiagNW, GetAllDiagSW, GetAllDiagSE, GetAllDiagNE ])))

-- Reachable square type families for each diagonal direction
data AllReachableDiagNW :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableDiagNW team board pos) = Eval (AllReachableFunc team board pos GetAllDiagNW)

data AllReachableDiagSW :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableDiagSW team board pos) = Eval (AllReachableFunc team board pos GetAllDiagSW)

data AllReachableDiagSE :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableDiagSE team board pos) = Eval (AllReachableFunc team board pos GetAllDiagSE)

data AllReachableDiagNE :: Team -> Board -> Position -> Exp [Position]
type instance Eval (AllReachableDiagNE team board pos) = Eval (AllReachableFunc team board pos GetAllDiagNE)

-- Prunes a list for all spaces taken up by a piece of the same team
-- (Perfect for kinds and knights!)
data AllReachableGivenList :: Team -> Board -> [Position] -> Exp [Position]
type instance Eval (AllReachableGivenList team board list) = Eval (Filter (FromMaybe True (Not . HasTeam team) . GetPieceAt board) list)

-- TODO: Reachable squares for L-shape (knights!)

-- General function, for taking the first N reachable positions from a particular direction.
-- NOTE: Relies on each directional function giving them in order of distance from the player
-- NOTE: Does not work with AllReachableDiag, as that will only be in one direction.
data NReachableFunc :: Team -> Board -> Position -> (Team -> Board -> Position -> Exp [Position]) -> Nat -> Exp [Position]
type instance Eval (NReachableFunc team board pos f n) = Eval (Take n (Eval (f team board pos)))

data NReachableDiagNW :: Team -> Board -> Position -> Nat -> Exp [Position]
type instance Eval (NReachableDiagNW team board pos n) = Eval (NReachableFunc team board pos AllReachableDiagNW n)

data NReachableDiagNE :: Team -> Board -> Position -> Nat -> Exp [Position]
type instance Eval (NReachableDiagNE team board pos n) = Eval (NReachableFunc team board pos AllReachableDiagNE n)

data NReachableDiagSW :: Team -> Board -> Position -> Nat -> Exp [Position]
type instance Eval (NReachableDiagSW team board pos n) = Eval (NReachableFunc team board pos AllReachableDiagSW n)

data NReachableDiagSE :: Team -> Board -> Position -> Nat -> Exp [Position]
type instance Eval (NReachableDiagSE team board pos n) = Eval (NReachableFunc team board pos AllReachableDiagSE n)

data NReachableBelow :: Team -> Board -> Position -> Nat -> Exp [Position]
type instance Eval (NReachableBelow team board pos n) = Eval (NReachableFunc team board pos AllReachableBelow n)

data NReachableAbove :: Team -> Board -> Position -> Nat -> Exp [Position]
type instance Eval (NReachableAbove team board pos n) = Eval (NReachableFunc team board pos AllReachableAbove n)

data NReachableLeft :: Team -> Board -> Position -> Nat -> Exp [Position]
type instance Eval (NReachableLeft team board pos n) = Eval (NReachableFunc team board pos AllReachableLeft n)

data NReachableRight :: Team -> Board -> Position -> Nat -> Exp [Position]
type instance Eval (NReachableRight team board pos n) = Eval (NReachableFunc team board pos AllReachableRight n)

-- The pawn is the only piece whose attack rules differ from its' movement rules;
-- so it requires a special case.
data PawnReachableAbove :: Board -> Position -> Nat -> Exp [Position]
type instance Eval (PawnReachableAbove board pos n) = Eval (GetFreePositions (Eval (NReachableAbove White board pos n)) board)

data PawnReachableBelow :: Board -> Position -> Nat -> Exp [Position]
type instance Eval (PawnReachableBelow board pos n) = Eval (GetFreePositions (Eval (NReachableBelow Black board pos n)) board)

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

-- Type families to add an offset to columns!
-- TODO: Customise the number of columns?? As it is, it's chess-specific.
-- TODO: Flip the arguments, they're the wrong way round!!
data (:+) :: MyNat -> Symbol -> Exp (Maybe Symbol)
data (:-) :: MyNat -> Symbol -> Exp (Maybe Symbol)

type instance Eval ((:+) Z         col) = Eval (ValidColumn col)
type instance Eval ((:+) (S Z)     "a") = Just "b"
type instance Eval ((:+) (S Z)     "b") = Just "c"
type instance Eval ((:+) (S Z)     "c") = Just "d"
type instance Eval ((:+) (S Z)     "d") = Just "e"
type instance Eval ((:+) (S Z)     "e") = Just "f"
type instance Eval ((:+) (S Z)     "f") = Just "g"
type instance Eval ((:+) (S Z)     "g") = Just "h"
type instance Eval ((:+) (S Z)     "h") = Nothing
type instance Eval ((:+) (S (S n)) col) = Eval (Bind ((:+) (S n)) (Eval ((:+) (S Z) col)))

type instance Eval ((:-) Z         col) = Eval (ValidColumn col)
type instance Eval ((:-) (S Z)     "a") = Nothing
type instance Eval ((:-) (S Z)     "b") = Just "a"
type instance Eval ((:-) (S Z)     "c") = Just "b"
type instance Eval ((:-) (S Z)     "d") = Just "c"
type instance Eval ((:-) (S Z)     "e") = Just "d"
type instance Eval ((:-) (S Z)     "f") = Just "e"
type instance Eval ((:-) (S Z)     "g") = Just "f"
type instance Eval ((:-) (S Z)     "h") = Just "g"
type instance Eval ((:-) (S (S n)) col) = Eval (Bind ((:-) (S n)) (Eval ((:-) (S Z) col)))

-- When using Maybes, this returns another maybe!
-- :kind! Eval (VecAt TestBoard Z) :: Maybe (Vec 8 (Maybe Piece))
-- data Bind :: (a -> Exp (f b)) -> f a -> Exp (f b)
data VecAt :: Vec n a -> MyNat -> Exp (Maybe a)
type instance Eval (VecAt VEnd _)           = Nothing
type instance Eval (VecAt (x :-> xs) Z)     = Just x
type instance Eval (VecAt (x :-> xs) (S n)) = Eval (VecAt xs n)

-- :kind! Eval (("a" :-> "b" :<> "c") !! (S (S Z))) = 'Just "c"
data (!!) :: Vec n a -> MyNat -> Exp (Maybe a)
type instance Eval (vec !! nat) = Eval (VecAt vec nat)

type family ElemIndex (vec :: Vec n a) (item :: a) :: Maybe Nat where
    ElemIndex VEnd item          = Nothing
    ElemIndex (item :-> xs) item = Just 0
    ElemIndex (x :-> xs)    item = Eval (Map (Add 1) (ElemIndex xs item))

-- TODO: Maybe make this tied less to ValidColumns??
type family ColToIndex (col :: Symbol) :: Maybe Nat where
    ColToIndex col = ElemIndex ValidColumns col

-- This checks for the validity of the position before it sends one off!
data GetPieceAt :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GetPieceAt board pos) = Eval (If (Eval (IsValidPosition pos)) (GetPieceAtNoChecks board pos) (ID Nothing))

data GetPieceAtNoChecks :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GetPieceAtNoChecks board (At col row)) = Eval (Join (Eval (Join (Eval ((Eval ((CW (!!)) <$> (Eval (board !! (Eval (NatToMyNat (row - 1))))))) <*> (Eval (NatToMyNat <$> (ColToIndex col))))))))

data IsPieceAt :: Board -> Position -> Exp Bool
type instance Eval (IsPieceAt board pos) = Eval (IsJust (Eval (GetPieceAt board pos)))

data GetFreePositions :: [Position] -> Board -> Exp [Position]
type instance Eval (GetFreePositions '[] _) = '[]
type instance Eval (GetFreePositions (p ': ps) board) = Eval (If (Eval ((Eval (IsPieceAt board p)) :||: ((Not . IsValidPosition) p))) (GetFreePositions ps board) (ID (p ': (Eval (GetFreePositions ps board)))))

-- This function just checks the spots a piece can move to; it does not handle moving itself.
-- That is in the other function, named Move.
-- TODO: Maybe represent the boards that the piece can move to? A new function, MovePiece, which handles any side effects??
-- Returns an empty list if the board is empty at that position!
-- NOTE: This allows pieces to state that they can move to the King's position; but this is just for check purposes. They can't actually take the king.
data CalculateValidMoves :: Position -> Board -> Exp [Position]
type instance Eval (CalculateValidMoves pos board) = Eval (FromMaybe '[] ((Flip PieceCanMoveTo) board) (Eval (GetPieceAt board pos)))

-- TODO: Check that the piece's reported position is its' actual position
-- TODO: Test all this!!! Near-urgently!
-- TODO: Allow for kinds to do castling
-- TODO: Allow for en passant takes
data PieceCanMoveTo :: Piece -> Board -> Exp [Position]
type instance Eval (PieceCanMoveTo (MkPiece team Pawn info) board)   = Eval (If (Eval ((IsZero . GetMoveCount) info)) (PawnStartMove (MkPiece team Pawn info) board) (PawnPostStart (MkPiece team Pawn info) board))
type instance Eval (PieceCanMoveTo (MkPiece team Bishop info) board) = Eval (AllReachableDiag team board (Eval (GetPosition info)))
type instance Eval (PieceCanMoveTo (MkPiece team Knight info) board) = Eval (AllReachableGivenList team board (Eval (GetAllKnightPositions (Eval (GetPosition info)))))
type instance Eval (PieceCanMoveTo (MkPiece team Rook info) board)   = Eval (AllReachableStraightLine team board (Eval (GetPosition info)))
type instance Eval (PieceCanMoveTo (MkPiece team Queen info) board)  = Eval (AllReachableLineAndDiag team board (Eval (GetPosition info)))
type instance Eval (PieceCanMoveTo (MkPiece team King info) board)   = Eval (AllReachableGivenList team board (Eval (GetAdjacent (Eval (GetPosition info)))))

-- Type family for where a pawn can move when it is in its' starting position
data PawnStartMove :: Piece -> Board -> Exp [Position]
type instance Eval (PawnStartMove pawn board) = Eval ((Eval (PawnMove pawn board 2)) ++ (Eval (PawnTakePositions pawn board)))

-- Type family for getting the initial pawn two-forward move!
-- TODO: Throw a type error if the Pawn has already moved??
data PawnMove :: Piece -> Board -> Nat -> Exp [Position]
type instance Eval (PawnMove (MkPiece Black Pawn info) board n) = Eval (PawnReachableBelow board (Eval (GetPosition info)) n)
type instance Eval (PawnMove (MkPiece White Pawn info) board n) = Eval (PawnReachableAbove board (Eval (GetPosition info)) n)

-- Pawns can take diagonally in front of themselves: so this gets those positions if a take is possible!
-- TODO: Handle "en passant" takes
data PawnTakePositions :: Piece -> Board -> Exp [Position]
type instance Eval (PawnTakePositions (MkPiece Black Pawn info) board) = Eval ((Eval (NReachableDiagSE Black board (Eval (GetPosition info)) 1)) ++ (Eval (NReachableDiagSW Black board (Eval (GetPosition info)) 1)))
type instance Eval (PawnTakePositions (MkPiece White Pawn info) board) = Eval ((Eval (NReachableDiagNE White board (Eval (GetPosition info)) 1)) ++ (Eval (NReachableDiagNW White board (Eval (GetPosition info)) 1)))

data PawnPostStart :: Piece -> Board -> Exp [Position]
type instance Eval (PawnPostStart pawn board) = Eval ((Eval (PawnMove pawn board 1)) ++ (Eval (PawnTakePositions pawn board)))

-- Type family for actually moving the piece, and handling the side effects.
-- TODO: Handle moves that can transform pieces (e.g. Pawn moving to the edge of the board)
-- TODO: Handle moves that can move multiple pieces (e.g. castling)
-- TODO: Handle takes (i.e. moves that remove pieces from play)
-- TODO: Ensure that pieces don't move to where the King is!
-- TODO: Move the piece/pieces, update those pieces' position info, increment those pieces' move count
data Move :: Position -> Board -> Exp (Maybe Board)
type instance Eval (Move pos board) = TypeError (Text "Not implemented!")

-----------------------------------------------------------------------------------------------

data SNat (n :: MyNat) where
    SZ :: SNat Z
    SS :: SNat n -> SNat (S n)

type family MyNatToSNat (n :: MyNat) :: SNat n where
    MyNatToSNat Z     = SZ
    MyNatToSNat (S n) = SS (MyNatToSNat n)

data MyNatLength :: [a] -> Exp MyNat
type instance Eval (MyNatLength '[]) = Z
type instance Eval (MyNatLength (x ': xs)) = S (Eval (MyNatLength xs))
