module Lib where

import GHC.TypeLits
import FirstClassFunctions
import Vec

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- A datatype for Proxy types!
data Proxy a = Proxy

-- Type synonym for an 8x8 grid
type Eight = S (S (S (S (S (S (S (S Z)))))))
type Row = Vec Eight (Maybe Piece)
type Grid8x8 = Vec Eight Row

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

-- TODO: Validity check??
data SetPosition :: PieceInfo -> Position -> Exp PieceInfo
type instance Eval (SetPosition (Info n _) pos) = Info n pos

data PieceMoveCount :: Piece -> Exp MyNat
type instance Eval (PieceMoveCount (MkPiece _ _ info)) = Eval (GetMoveCount info)

data PiecePosition :: Piece -> Exp Position
type instance Eval (PiecePosition (MkPiece _ _ info)) = Eval (GetPosition info)

data SetPiecePosition :: Piece -> Position -> Exp Piece
type instance Eval (SetPiecePosition (MkPiece t n info) pos) = MkPiece t n (Eval (SetPosition info pos))

data PieceTeam :: Piece -> Exp Team
type instance Eval (PieceTeam (MkPiece team _ _)) = team

data PieceType :: Piece -> Exp PieceName
type instance Eval (PieceType (MkPiece _ name _)) = name

data NoOfPieces :: Board -> Exp Nat
type instance Eval (NoOfPieces board) = Eval (Foldr Plus 0 (Eval ((VFilterCount IsJust) <$> board)))

-- TODO: Type level char??
-- Goes column-row, e.g. At "a" 4 means first column from left, 4 up from the bottom, where Black is at the top
data Position where
    At :: Symbol -> Nat -> Position

-- TODO: Implement TypeShow for type level naturals??
type instance TypeShow (At col row) = "At " ++ col ++ " (" ++ TypeShow row ++ ")"

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
type instance Eval (GetAllKnightPositions pos) = Eval (Filter IsValidPosition (Eval (GetKnightAboveBelow pos) ++ Eval (GetKnightLeftRight pos)))

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
type instance Eval (AllReachableLineAndDiag team board pos) = (Eval (AllReachableStraightLine team board pos)) ++ (Eval (AllReachableDiag team board pos))

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
-- :kind! Eval (VecAt TestBoard Z) :: Maybe Row
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

data PutAt :: a -> MyNat -> Vec n a -> Exp (Vec n a)
type instance Eval (PutAt x Z (y :-> ys))     = x :-> ys
type instance Eval (PutAt x (S n) (y :-> ys)) = y :-> Eval (PutAt x n ys)

-- TODO: Maybe make this tied less to ValidColumns??
type family ColToIndex (col :: Symbol) :: Maybe Nat where
    ColToIndex col = ElemIndex ValidColumns col

-- This checks for the validity of the position before it sends one off!
data GetPieceAt :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GetPieceAt board pos) = Eval (If (Eval (IsValidPosition pos)) (GetPieceAtNoChecks board pos) (ID Nothing))

data GetPieceAtNoChecks :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GetPieceAtNoChecks board (At col row)) = Eval (Join (Eval (Join (Eval ((Eval ((CW (!!)) <$> (Eval (GetRow board row)))) <*> (Eval (NatToMyNat <$> (ColToIndex col))))))))

data SetPieceAt :: Piece -> Board -> Position -> Exp Board
type instance Eval (SetPieceAt piece board pos) = Eval (If (Eval (IsValidPosition pos)) (SetPieceAtNoChecks piece board pos) (ID board))

data SetPieceAtNoChecks :: Piece -> Board -> Position -> Exp Board
type instance Eval (SetPieceAtNoChecks piece board (At col row)) = Eval (SetRow board row (Eval (PutAt (Just (Eval (SetPiecePosition piece (At col row)))) (Eval (NatToMyNat (Eval (FromJust (ColToIndex col))))) (Eval (FromJust (Eval (GetRow board row)))))))

data GetRow :: Board -> Nat -> Exp (Maybe Row)
type instance Eval (GetRow board n) = Eval (board !! Eval (NatToMyNat (n - 1)))

-- Uses 1 for first row, and 8 for last row!
data SetRow :: Board -> Nat -> Row -> Exp Board
type instance Eval (SetRow board n row) = Eval (PutAt row (Eval (NatToMyNat (n - 1))) board)

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
type instance Eval (CalculateValidMoves pos board) = Eval (FromMaybe '[] ((Flip PieceMoveList) board) (Eval (GetPieceAt board pos)))

-- TODO: Check that the piece's reported position is its' actual position
-- TODO: Test all this!!! Near-urgently!
-- TODO: Allow for kinds to do castling
-- TODO: Allow for en passant takes
data PieceMoveList :: Piece -> Board -> Exp [Position]
type instance Eval (PieceMoveList (MkPiece team Pawn info) board)   = Eval (If (Eval ((IsZero . GetMoveCount) info)) (PawnStartMove (MkPiece team Pawn info) board) (PawnPostStart (MkPiece team Pawn info) board))
type instance Eval (PieceMoveList (MkPiece team Bishop info) board) = Eval (AllReachableDiag team board (Eval (GetPosition info)))
type instance Eval (PieceMoveList (MkPiece team Knight info) board) = Eval (AllReachableGivenList team board (Eval (GetAllKnightPositions (Eval (GetPosition info)))))
type instance Eval (PieceMoveList (MkPiece team Rook info) board)   = Eval (AllReachableStraightLine team board (Eval (GetPosition info)))
type instance Eval (PieceMoveList (MkPiece team Queen info) board)  = Eval (AllReachableLineAndDiag team board (Eval (GetPosition info)))
type instance Eval (PieceMoveList (MkPiece team King info) board)   = Eval (AllReachableGivenList team board (Eval (GetAdjacent (Eval (GetPosition info)))))

data CanMoveTo :: Piece -> Position -> Board -> Exp Bool
type instance Eval (CanMoveTo piece pos board) = Eval (pos `In` Eval (PieceMoveList piece board))

-- Type family for where a pawn can move when it is in its' starting position
-- TODO: Throw a type error if the Pawn has already moved??
data PawnStartMove :: Piece -> Board -> Exp [Position]
type instance Eval (PawnStartMove pawn board) = (Eval (PawnMove pawn board 2)) ++ (Eval (PawnTakePositions pawn board))

-- Type family for getting the initial pawn two-forward move!
data PawnMove :: Piece -> Board -> Nat -> Exp [Position]
type instance Eval (PawnMove (MkPiece Black Pawn info) board n) = Eval (PawnReachableBelow board (Eval (GetPosition info)) n)
type instance Eval (PawnMove (MkPiece White Pawn info) board n) = Eval (PawnReachableAbove board (Eval (GetPosition info)) n)

-- Pawns can take diagonally in front of themselves: so this gets those positions if a take is possible!
-- TODO: Handle "en passant" takes
data PawnTakePositions :: Piece -> Board -> Exp [Position]
type instance Eval (PawnTakePositions (MkPiece Black Pawn info) board) = Eval (NReachableDiagSE Black board (Eval (GetPosition info)) 1) ++ (Eval (NReachableDiagSW Black board (Eval (GetPosition info)) 1))
type instance Eval (PawnTakePositions (MkPiece White Pawn info) board) = Eval (NReachableDiagNE White board (Eval (GetPosition info)) 1) ++ (Eval (NReachableDiagNW White board (Eval (GetPosition info)) 1))

data PawnPostStart :: Piece -> Board -> Exp [Position]
type instance Eval (PawnPostStart pawn board) = (Eval (PawnMove pawn board 1)) ++ (Eval (PawnTakePositions pawn board))

-- Type family for actually moving the piece, and handling the side effects.
-- TODO: Handle moves that can transform pieces (e.g. Pawn moving to the edge of the board)
-- TODO: Handle moves that can move multiple pieces (e.g. castling)
-- TODO: Handle takes (i.e. moves that remove pieces from play)
-- TODO: Ensure that pieces don't move to where the King is!
-- TODO: Move the piece/pieces, update those pieces' position info, increment those pieces' move count
data Move :: Position -> Position -> Board -> Exp (Maybe Board)
type instance Eval (Move fromPos toPos board) = Eval (If (Eval (IsPieceAt board fromPos)) (TE' (Text "Have not implemented Move yet!")) (TE' (Text ("There is no piece at: " ++ TypeShow fromPos))))

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

-- type EmptyRow     = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing
-- type EmptyBoard = EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :<> EmptyRow