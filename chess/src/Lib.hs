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
-- TODO: If required, allow curry wrapping at multiple layers, for futype (???) = TypeError (Text "Function undefined!")nctions like (a -> b -> c) ??
data CurryWrap :: (a -> b) -> a -> Exp b
type instance Eval (CurryWrap f a) = f a
data CW :: (a -> b) -> a -> Exp b
type instance Eval (CW f a) = Eval (CurryWrap f a)

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
data Pure :: a -> Exp (f a)
type instance Eval (Pure x) = Just x

data Apply :: f (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Apply _ Nothing)         = Nothing
type instance Eval (Apply (Just f) (Just x)) = Just (Eval (f x))

data (<*>) :: f (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (f <*> x) = Eval (Apply f x)

-- Type-level monads! (Almost)
data Return :: a -> Exp (f a)
type instance Eval (Return x) = Pure x

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
data Holds :: [a -> Exp Bool] -> a -> Exp Bool
type instance Eval (Holds '[] x)       = True
type instance Eval (Holds (f ': fs) x) = Eval ((Eval (f x)) :&&: (Holds fs x))


-----------------------------------------------------------------------------------------------
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

data Any :: (a -> Exp Bool) -> Vec n a -> Exp Bool
type instance Eval (Any p VEnd)       = False
type instance Eval (Any p (x :-> xs)) = Eval (Eval (p x) :||: Any p xs)

data All :: (a -> Exp Bool) -> Vec n a -> Exp Bool
type instance Eval (All p VEnd)       = True
type instance Eval (All p (x :-> xs)) = Eval (Eval (p x) :&&: All p xs)

data Length :: t a -> Exp Nat
type instance Eval (Length VEnd)       = 0
type instance Eval (Length (x :-> xs)) = 1 + Eval (Length xs)
type instance Eval (Length '[])        = 0
type instance Eval (Length (x ': xs))  = 1 + Eval (Length xs)

-- TODO: FilterMap instance for vectors??
data FilterMap :: (a -> Exp Bool) -> (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (FilterMap p f (x ': xs)) = Eval (If (Eval (p x)) (ID (Eval (f x) ': Eval (FilterMap p f xs))) (FilterMap p f xs))
type instance Eval (FilterMap p f '[])       = '[]

-- TODO: Kind-polymorphic Filter instances??
data Filter :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (Filter p '[]) = '[]
type instance Eval (Filter p (x ': xs)) = Eval (If (Eval (p x)) (ID (x ': Eval (Filter p xs))) (Filter p xs))

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

-- TODO: Type level char??
-- Goes column-row, e.g. At "a" 4 means first column from left, 4 up from the bottom, where Black is at the top
data Position where
    At :: Symbol -> Nat -> Position

type ValidColumns = "a" :-> "b" :-> "c" :-> "d" :-> "e" :-> "f" :-> "g" :<> "h"

type ValidRows = 1 :-> 2 :-> 3 :-> 4 :-> 5 :-> 6 :-> 7 :<> 8

-- type family ValidColumn (row :: Symbol) :: Maybe Symbol where
--     ValidColumn x = Eval (If (Elem x ValidColumns) (ID (Just x)) (ID Nothing))

data ValidColumn :: Symbol -> Exp (Maybe Symbol)
type instance Eval (ValidColumn x) = Eval (If (Elem x ValidColumns) (ID (Just x)) (ID Nothing))

data IsValidColumn :: Symbol -> Exp Bool
type instance Eval (IsValidColumn x) = Eval (IsJust (Eval (ValidColumn x)))

data IsValidRow :: Nat -> Exp Bool
type instance Eval (IsValidRow x) = Eval (If (Elem x ValidRows) (ID True) (ID False))

data IsValidPosition :: Position -> Exp Bool
type instance Eval (IsValidPosition (At col row)) = Eval ((Eval (IsValidColumn col)) :&&: (IsValidRow row))

-- -- FIXME: This can go out of bounds, and cause an evaluation problem (nats cannot go below 0)
-- data GetNBelow :: Position -> MyNat -> Exp [Position]
-- type instance Eval (GetNBelow p n) = Eval (Filter IsValidPosition (Eval (GetNBelowNoChecks p n)))

-- data GetNBelowNoChecks :: Position -> MyNat -> Exp [Position]
-- type instance Eval (GetNBelowNoChecks (At col row) Z)     = '[]
-- type instance Eval (GetNBelowNoChecks (At col row) (S n)) = (At col (row - (Eval (MyNatToNat (S n))))) ': Eval (GetNBelowNoChecks (At col row) n)

data GetTwoBelow :: Position -> Exp [Position]
type instance Eval (GetTwoBelow (At col row)) = Eval (Filter IsValidPosition [At col (row - 1), At col (row - 2)])

data GetTwoAbove :: Position -> Exp [Position]
type instance Eval (GetTwoAbove (At col row)) = Eval (Filter IsValidPosition [At col (row + 1), At col (row + 2)])

data IsOpposingTeam :: Piece -> Piece -> Exp Bool
type instance Eval (IsOpposingTeam (MkPiece White _ _) (MkPiece White _ _)) = False
type instance Eval (IsOpposingTeam (MkPiece Black _ _) (MkPiece Black _ _)) = False
type instance Eval (IsOpposingTeam (MkPiece White _ _) (MkPiece Black _ _)) = True
type instance Eval (IsOpposingTeam (MkPiece Black _ _) (MkPiece White _ _)) = True

data IsSameTeam :: Piece -> Piece -> Exp Bool
type instance Eval (IsSameTeam p1 p2) = Eval (Not (Eval (IsOpposingTeam p1 p2)))

-- Custom Nat class, to allow pattern matching on Nat > 2
data MyNat where
    Z :: MyNat
    S :: MyNat -> MyNat

data IsZero :: MyNat -> Exp Bool
type instance Eval (IsZero Z)     = True
type instance Eval (IsZero (S n)) = False

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

-- TEST TYPES
-- TODO: Remove these
type TestPosition = At "a" 1  -- i.e. bottom left
type TestPiece    = MkPiece Black Pawn (Info Z TestPosition)
type EmptyRow     = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing
type TestBoard    = (Just TestPiece :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :<> EmptyRow

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

-- TODO: Make it cause an error when row = 0
data GetPieceAt :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GetPieceAt board (At col row)) = Eval (Join (Eval (Join (Eval ((Eval ((CW (!!)) <$> (Eval (board !! (Eval (NatToMyNat (row - 1))))))) <*> (Eval (NatToMyNat <$> (ColToIndex col))))))))

data IsPieceAt :: Board -> Position -> Exp Bool
type instance Eval (IsPieceAt board pos) = Eval (IsJust (Eval (GetPieceAt board pos)))

data IsBlack :: Piece -> Exp Bool
type instance Eval (IsBlack (MkPiece team _ _)) = Eval (team :==: Black)

data IsWhite :: Piece -> Exp Bool
type instance Eval (IsWhite (MkPiece team _ _)) = Eval (team :==: White)

data GetFreePositions :: [Position] -> Board -> Exp [Position]
type instance Eval (GetFreePositions '[] _) = '[]
type instance Eval (GetFreePositions (p ': ps) board) = Eval (If (Eval ((Eval (IsPieceAt board p)) :||: ((Not . IsValidPosition) p))) (GetFreePositions ps board) (ID (p ': (Eval (GetFreePositions ps board)))))

-- TODO: Figure out how to handle the side effects of moves (e.g. taking a piece, castling, replacing a piece with another)
-- TODO: Maybe represent the boards that the piece can move to? A new function, MovePiece, which handles any side effects??
-- Returns an empty vector if the board is empty at that position!
data CalculateValidMoves :: Position -> Board -> Exp [Position]
type instance Eval (CalculateValidMoves pos board) = Eval (FromMaybe '[] ((Flip PieceCanMoveTo) board) (Eval (GetPieceAt board pos)))

-- TODO: Write instances for each team x piece, e.g. White Pawn, Black Knight, ...
-- TODO: Check that the piece's reported position is its' actual position
data PieceCanMoveTo :: Piece -> Board -> Exp [Position]
-- type instance Eval (PieceCanMoveTo (MkPiece Black Pawn info) board)   = Eval (If (Eval ((IsZero . GetMoveCount) info)) () ())
type instance Eval (PieceCanMoveTo (MkPiece White Pawn info) board)   = TypeError (Text "Not written PieceCanMoveTo yet!")
type instance Eval (PieceCanMoveTo (MkPiece Black Bishop info) board) = TypeError (Text "Not written PieceCanMoveTo yet!")
type instance Eval (PieceCanMoveTo (MkPiece White Bishop info) board) = TypeError (Text "Not written PieceCanMoveTo yet!")
type instance Eval (PieceCanMoveTo (MkPiece Black Knight info) board) = TypeError (Text "Not written PieceCanMoveTo yet!")
type instance Eval (PieceCanMoveTo (MkPiece White Knight info) board) = TypeError (Text "Not written PieceCanMoveTo yet!")
type instance Eval (PieceCanMoveTo (MkPiece Black Rook info) board)   = TypeError (Text "Not written PieceCanMoveTo yet!")
type instance Eval (PieceCanMoveTo (MkPiece White Rook info) board)   = TypeError (Text "Not written PieceCanMoveTo yet!")
type instance Eval (PieceCanMoveTo (MkPiece Black Queen info) board)  = TypeError (Text "Not written PieceCanMoveTo yet!")
type instance Eval (PieceCanMoveTo (MkPiece White Queen info) board)  = TypeError (Text "Not written PieceCanMoveTo yet!")
type instance Eval (PieceCanMoveTo (MkPiece Black King info) board)   = TypeError (Text "Not written PieceCanMoveTo yet!")
type instance Eval (PieceCanMoveTo (MkPiece White King info) board)   = TypeError (Text "Not written PieceCanMoveTo yet!")

-- TODO: Handle moves that can transform pieces (e.g. Pawn moving to the edge of the board)
-- TODO: Handle moves that can move multiple pieces (e.g. castling)
-- TODO: Handle takes (i.e. moves that remove pieces from play)
-- TODO: Move the piece/pieces, update those pieces' position info, increment those pieces' move count
data Move :: Position -> Board -> Exp (Maybe Board)
type instance Eval (Move pos board) = TypeError (Text "Not implemented!")

getPieceAtTest1 :: Proxy (Just TestPiece)
getPieceAtTest1 = Proxy @(Eval (GetPieceAt TestBoard (At "a" 1)))

-- :k VecAtR Z :: Vec n a -> Exp (Maybe a)
getPieceAtTest2 :: Proxy (Just TestPiece)
getPieceAtTest2 = Proxy @(Eval (Join (Eval (Bind ((Flip (!!)) (Eval (NatToMyNat 0))) (Eval (TestBoard !! (Eval (NatToMyNat 0))))))))

-- :kind! VecAt (Z :<> (S Z)) :: MyNat -> Exp (Maybe MyNat)
getPieceAtTest3 :: Proxy (Just Z)
getPieceAtTest3 = Proxy @(Eval (Join (Eval ((Eval ((CW (!!)) <$> Just (Z :<> (S Z)))) <*> Just Z))))

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

type TestList = '["a","b","c"]
