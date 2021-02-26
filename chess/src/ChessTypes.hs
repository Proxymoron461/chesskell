module ChessTypes where

import qualified GHC.TypeLits as TL
import FirstClassFunctions
import Vec
import Data.Type.Nat hiding (SNat(..))

-- Type synonym for an 8x8 grid
type Row = Vec Eight (Maybe Piece)
type Grid8x8 = Vec Eight Row

-- This delays the evaluation of the type error!
-- (Thanks https://blog.poisson.chat/posts/2018-08-06-one-type-family.html#fnref4)
data TE' :: TL.ErrorMessage -> Exp a
type instance Eval (TE' msg) = TL.TypeError msg

-- TODO: Dimensions of board in kind??
type Board = Grid8x8

data Piece where
    MkPiece :: Team -> PieceName -> PieceInfo -> Piece

data Team = Black | White

type instance TypeShow Black = "Black"
type instance TypeShow White = "White"

data OppositeTeam :: Team -> Exp Team
type instance Eval (OppositeTeam team) = OppositeTeam' team

type family OppositeTeam' (t :: Team) :: Team where
    OppositeTeam' White = Black
    OppositeTeam' Black = White

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

data PromoteTo :: PieceName -> Piece -> Exp Piece
type instance Eval (PromoteTo name (MkPiece t _ i)) = MkPiece t name i

-- Make singleton types for each piece??
data PieceName = Pawn
               | Bishop
               | Knight
               | Rook
               | King
               | Queen

type instance TypeShow Pawn   = "Pawn"
type instance TypeShow Bishop = "Bishop"
type instance TypeShow Knight = "Knight"
type instance TypeShow Rook   = "Rook"
type instance TypeShow Queen  = "Queen"
type instance TypeShow King   = "King"

-- Holds the number of moves they've made, plus their current position.
-- While their position is implicit from where they are in the board, it's
-- helpful!
-- TODO: Remove Bool entry!!
data PieceInfo where
    Info :: Nat -> Position -> PieceInfo

data GetMoveCount :: PieceInfo -> Exp Nat
type instance Eval (GetMoveCount (Info x _)) = x

data HasMoveCount :: Nat -> PieceInfo -> Exp Bool
type instance Eval (HasMoveCount n (Info x _)) = Eval (n :==: x)

data GetPosition :: PieceInfo -> Exp Position
type instance Eval (GetPosition info) = GetPosition' info

type family GetPosition' (p :: PieceInfo) :: Position where
   GetPosition' (Info _ x) = x

-- New datatype to hold the board, as well as some intermediate state, including:

data BoardDecorator where
    Dec :: Board -> Team -> Position -> (Position, Position) -> Nat -> BoardDecorator

type family GetBoard (x :: BoardDecorator) :: Board where
    GetBoard (Dec b _ _ _ _) = b

type family SetBoard (b :: Board) (x :: BoardDecorator) :: BoardDecorator where
   SetBoard b (Dec _ w x y z) = Dec b w x y z

type family GetLastPosition (x :: BoardDecorator) :: Position where
    GetLastPosition (Dec _ _ p _ _) = p

type family SetLastPosition (x :: Position) (y :: BoardDecorator) :: BoardDecorator where
   SetLastPosition pos (Dec w x p y z) = Dec w x pos y z

type family GetLastTeam (x :: BoardDecorator) :: Team where
   GetLastTeam (Dec _ t _ _ _) = t

type family SetLastTeam (x :: BoardDecorator) (y :: Team) :: BoardDecorator where
   SetLastTeam (Dec w _ y z a) x = Dec w x y z a

type family GetMovingTeam (x :: BoardDecorator) :: Team where
   GetMovingTeam boardDec = OppositeTeam' (GetLastTeam boardDec)

type family GetKingPosition (t :: Team) (x :: BoardDecorator) :: Position where
   GetKingPosition White (Dec _ _ _ '(white, _) _) = white
   GetKingPosition Black (Dec _ _ _ '(_, black) _) = black

type family UpdateKings (x :: (Position, Position)) (y :: Piece) (z :: Position) :: (Position, Position) where
   UpdateKings '(whitePos, _) (MkPiece Black King _) toPos = '(whitePos, toPos)
   UpdateKings '(_, blackPos) (MkPiece White King _) toPos = '(toPos, blackPos)
   UpdateKings kings          _                      _     = kings

type family GetKings (x :: BoardDecorator) :: (Position, Position) where
   GetKings (Dec _ _ _ k _) = k

type family SetKings (x :: (Position, Position)) (y :: BoardDecorator) :: BoardDecorator where
   SetKings '(whitePos, blackPos) (Dec w x y _ z) = Dec w x y '(whitePos, blackPos) Z

-- Should always start at Nat1
type family GetNoOfMoves (x :: BoardDecorator) :: Nat where
   GetNoOfMoves (Dec _ _ _ _ x) = x

-- TODO: Validity check??
data SetPosition :: PieceInfo -> Position -> Exp PieceInfo
type instance Eval (SetPosition (Info n _) pos) = Info n pos

data InfoIncrementMoves :: PieceInfo -> Exp PieceInfo
type instance Eval (InfoIncrementMoves (Info n pos)) = Info (S n) pos

data IncrementMoves :: Piece -> Exp Piece
type instance Eval (IncrementMoves (MkPiece team name info)) = MkPiece team name (Eval (InfoIncrementMoves info))

data PieceMoveCount :: Piece -> Exp Nat
type instance Eval (PieceMoveCount (MkPiece _ _ info)) = Eval (GetMoveCount info)

data PieceHasMoveCount :: Nat -> Piece -> Exp Bool
type instance Eval (PieceHasMoveCount n (MkPiece _ _ info)) = Eval (HasMoveCount n info)

data PiecePosition :: Piece -> Exp Position
type instance Eval (PiecePosition (MkPiece _ _ info)) = Eval (GetPosition info)

data LastPieceToMove :: BoardDecorator -> Piece -> Exp Bool
type instance Eval (LastPieceToMove boardDec piece) = Eval ((Eval (PiecePosition piece)) :==: (GetLastPosition boardDec))

data IsLastPieceMovedAt :: Position -> BoardDecorator -> Exp Bool
type instance Eval (IsLastPieceMovedAt pos boardDec) = Eval (pos :==: GetLastPosition boardDec)

data SetLastPieceMoved :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (SetLastPieceMoved pos boardDec) = SetLastPosition pos boardDec

data SetPiecePosition :: Piece -> Position -> Exp Piece
type instance Eval (SetPiecePosition (MkPiece t n info) pos) = MkPiece t n (Eval (SetPosition info pos))

data PieceTeam :: Piece -> Exp Team
type instance Eval (PieceTeam (MkPiece team _ _)) = team

data PieceType :: Piece -> Exp PieceName
type instance Eval (PieceType (MkPiece _ name _)) = name

data NoOfPieces :: Board -> Exp Nat
type instance Eval (NoOfPieces board) = Eval (Foldr FCFPlus Nat0 (Eval ((VFilterCount IsJust) <$> board)))

data IsPiece :: PieceName -> Piece -> Exp Bool
type instance Eval (IsPiece name (MkPiece _ pcName _)) = Eval (name :==: pcName)

data IsPawn :: Piece -> Exp Bool
type instance Eval (IsPawn piece) = Eval (IsPiece Pawn piece)

data IsBishop :: Piece -> Exp Bool
type instance Eval (IsBishop piece) = Eval (IsPiece Bishop piece)

data IsKnight :: Piece -> Exp Bool
type instance Eval (IsKnight piece) = Eval (IsPiece Knight piece)

data IsRook :: Piece -> Exp Bool
type instance Eval (IsRook piece) = Eval (IsPiece Rook piece)

data IsKing :: Piece -> Exp Bool
type instance Eval (IsKing piece) = Eval (IsPiece King piece)

data IsQueen :: Piece -> Exp Bool
type instance Eval (IsQueen piece) = Eval (IsPiece Queen piece)

data Column = A | B | C | D | E | F | G | H
type instance TypeShow A = "A"
type instance TypeShow B = "B"
type instance TypeShow C = "C"
type instance TypeShow D = "D"
type instance TypeShow E = "E"
type instance TypeShow F = "F"
type instance TypeShow G = "G"
type instance TypeShow H = "H"

-- Goes column-row, e.g. At A 4 means first column from left, 4 up from the bottom, where Black is at the top
data Position where
    At :: Column -> Nat -> Position

type instance TypeShow (At col row) = TypeShow col ++ TypeShow row

data IsValidRow :: Nat -> Exp Bool
type instance Eval (IsValidRow x) = IsValidRowNonFCF x

type family IsValidRowNonFCF (x :: Nat) :: Bool where
   IsValidRowNonFCF Nat1 = True
   IsValidRowNonFCF Nat2 = True
   IsValidRowNonFCF Nat3 = True
   IsValidRowNonFCF Nat4 = True
   IsValidRowNonFCF Nat5 = True
   IsValidRowNonFCF Nat6 = True
   IsValidRowNonFCF Nat7 = True
   IsValidRowNonFCF Nat8 = True
   IsValidRowNonFCF _ = False

data IsValidPosition :: Position -> Exp Bool
type instance Eval (IsValidPosition (At col row)) = Eval (IsValidRow row)

-- This checks for the validity of the position before it sends one off!
data GetPieceAt :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GetPieceAt board pos) = Eval (If (Eval (IsValidPosition pos)) (GPANCUgly board pos) (ID Nothing))

data GetPieceAtNoChecks :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GetPieceAtNoChecks board (At col row)) = Eval (Join (Eval (Join (Eval ((Eval ((CW (!!)) <$> (Eval (GetRow board row)))) <*> (Just ((ColToIndex col))))))))

data GetPieceAtDec :: BoardDecorator -> Position -> Exp (Maybe Piece)
type instance Eval (GetPieceAtDec boardDec pos) = Eval (GetPieceAt (GetBoard boardDec) pos)

data GPANCUgly :: Board -> Position -> Exp (Maybe Piece)
type instance Eval (GPANCUgly (a :-> xs) (At col Nat1)) = VecAt a (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c) (At col Nat2)) = VecAt b (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d) (At col Nat3)) = VecAt c (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d :-> e) (At col Nat4)) = VecAt d (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d :-> e :-> f) (At col Nat5)) = VecAt e (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d :-> e :-> f :-> g) (At col Nat6)) = VecAt f (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h) (At col Nat7)) = VecAt g (ColToIndex col)
type instance Eval (GPANCUgly (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h :-> xs) (At col Nat8)) = VecAt h (ColToIndex col)

data GetPieceAtWhich :: Board -> Position -> (a -> Exp Bool) -> Exp (Maybe Piece)
type instance Eval (GetPieceAtWhich board pos f) = Eval (MaybeWhich f (Eval (GetPieceAt board pos)))

data GetPieceAtWhichDec :: BoardDecorator -> Position -> (a -> Exp Bool) -> Exp (Maybe Piece)
type instance Eval (GetPieceAtWhichDec boardDec pos f) = Eval (GetPieceAtWhich (GetBoard boardDec) pos f)

data IsPieceAtWhich :: Board -> Position -> (a -> Exp Bool) -> Exp Bool
type instance Eval (IsPieceAtWhich board pos f) = Eval ((IsJust . (GetPieceAtWhich board pos)) f)

data IsPieceAtWhichDec :: BoardDecorator -> Position -> (a -> Exp Bool) -> Exp Bool
type instance Eval (IsPieceAtWhichDec boardDec pos f) = Eval (IsPieceAtWhich (GetBoard boardDec) pos f)

data ApplyFuncAt :: (Piece -> Exp Piece) -> Board -> Position -> Exp Board
type instance Eval (ApplyFuncAt f board pos) = Eval (FromMaybe board ((FlipToLast SetPieceAt) board pos . f) (Eval (GetPieceAt board pos)))

data SetPieceAt :: Piece -> Board -> Position -> Exp Board
type instance Eval (SetPieceAt piece board pos) = Eval (If (Eval (IsValidPosition pos)) (SetPieceAtNoChecks piece board pos) (ID board))
data SetPieceAtSwapped :: Piece -> Position -> Board -> Exp Board
type instance Eval (SetPieceAtSwapped piece pos board) = Eval (SetPieceAt piece board pos)

data PromotePieceTo :: PieceName -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (PromotePieceTo name pos b) = PromotePieceTo' name pos b

type family PromotePieceTo' (name :: PieceName) (pos :: Position) (b :: BoardDecorator) :: BoardDecorator where
   PromotePieceTo' Pawn _ boardDec   = TL.TypeError ((TL.Text "A Pawn cannot be promoted to a Pawn.") TL.:$$: (TL.Text ("Error at move: " ++ TypeShow (GetNoOfMoves boardDec))))
   PromotePieceTo' King _ boardDec   = TL.TypeError ((TL.Text "A Pawn cannot be promoted to a King.") TL.:$$: (TL.Text ("Error at move: " ++ TypeShow (GetNoOfMoves boardDec))))
   PromotePieceTo' name (At col Nat8) boardDec
      = Eval (If (Eval (IsPieceAtWhichDec boardDec (At col Nat8) (IsPawn .&. HasTeam White)))
            (ID (SetBoard (Eval (ApplyFuncAt (PromoteTo name) (GetBoard boardDec) (At col Nat8))) boardDec))
            (TE' (TL.Text ("The only promotable pieces in row 8 are White Pawns."))))
   PromotePieceTo' name (At col Nat1) boardDec
      = Eval (If (Eval (IsPieceAtWhichDec boardDec (At col Nat1) (IsPawn .&. HasTeam Black)))
            (ID (SetBoard (Eval (ApplyFuncAt (PromoteTo name) (GetBoard boardDec) (At col Nat1))) boardDec))
            (TE' (TL.Text ("The only promotable pieces in row 1 are Black Pawns."))))
   PromotePieceTo' _ (At col row) boardDec = TL.TypeError ((TL.Text ("Pawns can only be promoted in rows 1 and 8, not row: " ++ TypeShow row)) TL.:$$: (TL.Text ("Error at move: " ++ TypeShow (GetNoOfMoves boardDec))))

data SetPieceAtDec :: Piece -> BoardDecorator -> Position -> Exp BoardDecorator
type instance Eval (SetPieceAtDec piece boardDec toPos)
   = SetKings (UpdateKings (GetKings boardDec) piece toPos) (SetBoard (Eval (SetPieceAt piece (GetBoard boardDec) toPos)) boardDec)

data SetPieceAtDecSwapped :: Piece -> Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (SetPieceAtDecSwapped piece pos boardDec) = Eval (SetPieceAtDec piece boardDec pos)

data SetPieceAtDecClear :: Piece -> BoardDecorator -> Position -> Exp BoardDecorator
type instance Eval (SetPieceAtDecClear piece boardDec pos)
   = Eval (ClearPieceAtDec (Eval (PiecePosition piece)) (Eval (SetPieceAtDec piece boardDec pos)))

data ClearPieceAt :: Position -> Board -> Exp Board
type instance Eval (ClearPieceAt (At col row) board) = Eval (SetRow board row (Eval (PutAt Nothing (ColToIndex col) (Eval (FromJust (Eval (GetRow board row)))))))

data ClearPieceAtDec :: Position -> BoardDecorator -> Exp BoardDecorator
type instance Eval (ClearPieceAtDec pos boardDec) = SetBoard (Eval (ClearPieceAt pos (GetBoard boardDec))) boardDec

-- TODO: Optimise to not use GetRow??
data SetPieceAtNoChecks :: Piece -> Board -> Position -> Exp Board
type instance Eval (SetPieceAtNoChecks piece board (At col row)) = Eval (SetRow board row (Eval (PutAt (Just (Eval (SetPiecePosition piece (At col row)))) (ColToIndex col) (Eval (FromJust (Eval (GetRow board row)))))))

-- TODO: Optimise to work in one fell swoop, rather than one by one?
data SetPiecesAt :: [(Piece, Position)] -> Board -> Exp Board
type instance Eval (SetPiecesAt pps board) = Eval (Foldr (Uncurry2 SetPieceAtSwapped) board pps)

data SetPiecesAtDec :: [(Piece, Position)] -> BoardDecorator -> Exp BoardDecorator
type instance Eval (SetPiecesAtDec pps boardDec) = Eval (Foldr (Uncurry2 SetPieceAtDecSwapped) boardDec pps)

data GetRow :: Board -> Nat -> Exp (Maybe Row)
type instance Eval (GetRow board (S n)) = Just $ VecAt board n

-- Uses 1 for first row, and 8 for last row!
data SetRow :: Board -> Nat -> Row -> Exp Board
type instance Eval (SetRow board (S n) row) = Eval (PutAt row n board)

type family SetRowDec' (b :: BoardDecorator) (n :: Nat) (r :: Row) :: BoardDecorator where
   SetRowDec' (Dec board team pos kings move) (S n) row = Dec (Eval (SetRow board (S n) row)) team pos kings move

-- Type families to add an offset to columns!
data (:+) :: Nat -> Column -> Exp (Maybe Column)
data (:-) :: Nat -> Column -> Exp (Maybe Column)

type instance Eval ((:+) Z         col) = Just col
type instance Eval ((:+) (S Z)     A)   = Just B
type instance Eval ((:+) (S Z)     B)   = Just C
type instance Eval ((:+) (S Z)     C)   = Just D
type instance Eval ((:+) (S Z)     D)   = Just E
type instance Eval ((:+) (S Z)     E)   = Just F
type instance Eval ((:+) (S Z)     F)   = Just G
type instance Eval ((:+) (S Z)     G)   = Just H
type instance Eval ((:+) (S Z)     H)   = Nothing
type instance Eval ((:+) (S (S n)) col) = Eval ((Eval ((:+) (S Z) col)) >>= ((:+) (S n)))

type instance Eval ((:-) Z         col) = Just col
type instance Eval ((:-) (S Z)     A)   = Nothing
type instance Eval ((:-) (S Z)     B)   = Just A
type instance Eval ((:-) (S Z)     C)   = Just B
type instance Eval ((:-) (S Z)     D)   = Just C
type instance Eval ((:-) (S Z)     E)   = Just D
type instance Eval ((:-) (S Z)     F)   = Just E
type instance Eval ((:-) (S Z)     G)   = Just F
type instance Eval ((:-) (S Z)     H)   = Just G
type instance Eval ((:-) (S (S n)) col) = Eval ((Eval ((:-) (S Z) col)) >>= ((:-) (S n)))

type family ColToIndex (col :: Column) :: Nat where
    ColToIndex A = Nat0
    ColToIndex B = Nat1
    ColToIndex C = Nat2
    ColToIndex D = Nat3
    ColToIndex E = Nat4
    ColToIndex F = Nat5
    ColToIndex G = Nat6
    ColToIndex H = Nat7

type family OneLeft (p :: Position) :: Position where
   OneLeft (At A   row) = TL.TypeError (TL.Text ("Cannot move one left from:" ++ TypeShow (At A row)))
   OneLeft (At col row) = At (FromJust' (Eval (Nat1 :- col))) row

type family TwoLeft (p :: Position) :: Position where
   TwoLeft (At A   row) = TL.TypeError (TL.Text ("Cannot move two left from:" ++ TypeShow (At A row)))
   TwoLeft (At B   row) = TL.TypeError (TL.Text ("Cannot move two left from:" ++ TypeShow (At B row)))
   TwoLeft (At col row) = At (FromJust' (Eval (Nat2 :- col))) row

type family OneRight (p :: Position) :: Position where
   OneRight (At H   row) = TL.TypeError (TL.Text ("Cannot move one right from:" ++ TypeShow (At H row)))
   OneRight (At col row) = At (FromJust' (Eval (Nat1 :+ col))) row

type family TwoRight (p :: Position) :: Position where
   TwoRight (At H   row) = TL.TypeError (TL.Text ("Cannot move two right from:" ++ TypeShow (At H row)))
   TwoRight (At G   row) = TL.TypeError (TL.Text ("Cannot move two right from:" ++ TypeShow (At G row)))
   TwoRight (At col row) = At (FromJust' (Eval (Nat2 :+ col))) row

type family OneDown (p :: Position) :: Position where
   OneDown (At col Nat1)    = TL.TypeError (TL.Text ("Cannot move one down from:" ++ TypeShow (At col Nat1)))
   OneDown (At col (S row)) = At col row

type family OneUp (p :: Position) :: Position where
   OneUp (At col Nat8) = TL.TypeError (TL.Text ("Cannot move one up from:" ++ TypeShow (At col Nat8)))
   OneUp (At col row)  = At col (S row)


-----------------------------------------------------------------------------------------------------

type LastMoveInfo = (Maybe Board, Team)

type StartInfo = 'Info Z (At A Z)

type EmptyRow   = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing
type EmptyBoard = EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :<> EmptyRow

type EmptyDec = Dec EmptyBoard Black (At A Nat1) '(At E Nat1, At E Nat8) Nat1

type JustKingsDec = Dec JustKings Black (At A Nat1) '(At E Nat1, At E Nat8) Nat1
type JustKings = (Nothing :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece White King (Info Z (At E Nat1)))) :-> Nothing :-> Nothing :<> Nothing)
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :-> EmptyRow
                 :<> (Nothing :-> Nothing :-> Nothing :-> Nothing :-> (Just (MkPiece Black King (Info Z (At E Nat8)))) :-> Nothing :-> Nothing :<> Nothing)

type StartDec = Dec StartBoard Black (At A Nat1) '(At E Nat1, At E Nat8) Nat1

type StartBoard = ('Just
     ('MkPiece
        'White
        'Rook
        ('Info Z ('At 'A (S Z))))
   ':-> ('Just
           ('MkPiece
              'White
              'Knight
              ('Info Z ('At 'B (S Z))))
         ':-> ('Just
                 ('MkPiece
                    'White
                    'Bishop
                    ('Info Z ('At 'C (S Z))))
               ':-> ('Just
                       ('MkPiece
                          'White
                          'Queen
                          ('Info Z ('At 'D (S Z))))
                     ':-> ('Just
                             ('MkPiece
                                'White
                                'King
                                ('Info Z ('At 'E (S Z))))
                           ':-> ('Just
                                   ('MkPiece
                                      'White
                                      'Bishop
                                      ('Info Z ('At 'F (S Z))))
                                 ':-> ('Just
                                         ('MkPiece
                                            'White
                                            'Knight
                                            ('Info
                                               Z
                                               ('At 'G (S Z))))
                                       ':-> ('Just
                                               ('MkPiece
                                                  'White
                                                  'Rook
                                                  ('Info
                                                     Z
                                                     ('At 'H (S Z))))
                                             ':-> 'VEnd))))))))
  ':-> (('Just
           ('MkPiece
              'White
              'Pawn
              ('Info
                 Z
                 ('At 'A (S (S Z)))))
         ':-> ('Just
                 ('MkPiece
                    'White
                    'Pawn
                    ('Info
                       Z
                       ('At 'B (S (S Z)))))
               ':-> ('Just
                       ('MkPiece
                          'White
                          'Pawn
                          ('Info
                             Z
                             ('At 'C (S (S Z)))))
                     ':-> ('Just
                             ('MkPiece
                                'White
                                'Pawn
                                ('Info
                                   Z
                                   ('At 'D (S (S Z)))))
                           ':-> ('Just
                                   ('MkPiece
                                      'White
                                      'Pawn
                                      ('Info
                                         Z
                                         ('At 'E (S (S Z)))))
                                 ':-> ('Just
                                         ('MkPiece
                                            'White
                                            'Pawn
                                            ('Info
                                               Z
                                               ('At 'F (S (S Z)))))
                                       ':-> ('Just
                                               ('MkPiece
                                                  'White
                                                  'Pawn
                                                  ('Info
                                                     Z
                                                     ('At
                                                        'G (S (S Z)))))
                                             ':-> ('Just
                                                     ('MkPiece
                                                        'White
                                                        'Pawn
                                                        ('Info
                                                           Z
                                                           ('At
                                                              'H
                                                              (S
                                                                 (S Z)))))
                                                   ':-> 'VEnd))))))))
        ':-> (('Nothing
               ':-> ('Nothing
                     ':-> ('Nothing
                           ':-> ('Nothing
                                 ':-> ('Nothing
                                       ':-> ('Nothing
                                             ':-> ('Nothing ':-> ('Nothing ':-> 'VEnd))))))))
              ':-> (('Nothing
                     ':-> ('Nothing
                           ':-> ('Nothing
                                 ':-> ('Nothing
                                       ':-> ('Nothing
                                             ':-> ('Nothing
                                                   ':-> ('Nothing ':-> ('Nothing ':-> 'VEnd))))))))
                    ':-> (('Nothing
                           ':-> ('Nothing
                                 ':-> ('Nothing
                                       ':-> ('Nothing
                                             ':-> ('Nothing
                                                   ':-> ('Nothing
                                                         ':-> ('Nothing
                                                               ':-> ('Nothing ':-> 'VEnd))))))))
                          ':-> (('Nothing
                                 ':-> ('Nothing
                                       ':-> ('Nothing
                                             ':-> ('Nothing
                                                   ':-> ('Nothing
                                                         ':-> ('Nothing
                                                               ':-> ('Nothing
                                                                     ':-> ('Nothing
                                                                           ':-> 'VEnd))))))))
                                ':-> (('Just
                                         ('MkPiece
                                            'Black
                                            'Pawn
                                            ('Info
                                               Z
                                               ('At
                                                  'A
                                                  (S
                                                     (S
                                                        (S
                                                           (S
                                                              (S
                                                                 (S
                                                                    (S Z))))))))))
                                       ':-> ('Just
                                               ('MkPiece
                                                  'Black
                                                  'Pawn
                                                  ('Info
                                                     Z
                                                     ('At
                                                        'B
                                                        (S
                                                           (S
                                                              (S
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             Z))))))))))
                                             ':-> ('Just
                                                     ('MkPiece
                                                        'Black
                                                        'Pawn
                                                        ('Info
                                                           Z
                                                           ('At
                                                              'C
                                                              (S
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (S
                                                                                   Z))))))))))
                                                   ':-> ('Just
                                                           ('MkPiece
                                                              'Black
                                                              'Pawn
                                                              ('Info
                                                                 Z
                                                                 ('At
                                                                    'D
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (S
                                                                                   (S
                                                                                      (S
                                                                                         Z))))))))))
                                                         ':-> ('Just
                                                                 ('MkPiece
                                                                    'Black
                                                                    'Pawn
                                                                    ('Info
                                                                       Z
                                                                       ('At
                                                                          'E
                                                                          (S
                                                                             (S
                                                                                (S
                                                                                   (S
                                                                                      (S
                                                                                         (S
                                                                                            (S
                                                                                               Z))))))))))
                                                               ':-> ('Just
                                                                       ('MkPiece
                                                                          'Black
                                                                          'Pawn
                                                                          ('Info
                                                                             Z
                                                                             ('At
                                                                                'F
                                                                                (S
                                                                                   (S
                                                                                      (S
                                                                                         (S
                                                                                            (S
                                                                                               (S
                                                                                                  (S
                                                                                                     Z))))))))))
                                                                     ':-> ('Just
                                                                             ('MkPiece
                                                                                'Black
                                                                                'Pawn
                                                                                ('Info
                                                                                   Z
                                                                                   ('At
                                                                                      'G
                                                                                      (S
                                                                                         (S
                                                                                            (S
                                                                                               (S
                                                                                                  (S
                                                                                                     (S
                                                                                                        (S
                                                                                                           Z))))))))))
                                                                           ':-> ('Just
                                                                                   ('MkPiece
                                                                                      'Black
                                                                                      'Pawn
                                                                                      ('Info
                                                                                         Z
                                                                                         ('At
                                                                                            'H
                                                                                            (S
                                                                                               (S
                                                                                                  (S
                                                                                                     (S
                                                                                                        (S
                                                                                                           (S
                                                                                                              (S
                                                                                                                 Z))))))))))
                                                                                 ':-> 'VEnd))))))))
                                      ':-> (('Just
                                               ('MkPiece
                                                  'Black
                                                  'Rook
                                                  ('Info
                                                     Z
                                                     ('At
                                                        'A
                                                        (S
                                                           (S
                                                              (S
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                Z)))))))))))
                                             ':-> ('Just
                                                     ('MkPiece
                                                        'Black
                                                        'Knight
                                                        ('Info
                                                           Z
                                                           ('At
                                                              'B
                                                              (S
                                                                 (S
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (S
                                                                                   (S
                                                                                      Z)))))))))))
                                                   ':-> ('Just
                                                           ('MkPiece
                                                              'Black
                                                              'Bishop
                                                              ('Info
                                                                 Z
                                                                 ('At
                                                                    'C
                                                                    (S
                                                                       (S
                                                                          (S
                                                                             (S
                                                                                (S
                                                                                   (S
                                                                                      (S
                                                                                         (S
                                                                                            Z)))))))))))
                                                         ':-> ('Just
                                                                 ('MkPiece
                                                                    'Black
                                                                    'Queen
                                                                    ('Info
                                                                       Z
                                                                       ('At
                                                                          'D
                                                                          (S
                                                                             (S
                                                                                (S
                                                                                   (S
                                                                                      (S
                                                                                         (S
                                                                                            (S
                                                                                               (S
                                                                                                  Z)))))))))))
                                                               ':-> ('Just
                                                                       ('MkPiece
                                                                          'Black
                                                                          'King
                                                                          ('Info
                                                                             Z
                                                                             ('At
                                                                                'E
                                                                                (S
                                                                                   (S
                                                                                      (S
                                                                                         (S
                                                                                            (S
                                                                                               (S
                                                                                                  (S
                                                                                                     (S
                                                                                                        Z)))))))))))
                                                                     ':-> ('Just
                                                                             ('MkPiece
                                                                                'Black
                                                                                'Bishop
                                                                                ('Info
                                                                                   Z
                                                                                   ('At
                                                                                      'F
                                                                                      (S
                                                                                         (S
                                                                                            (S
                                                                                               (S
                                                                                                  (S
                                                                                                     (S
                                                                                                        (S
                                                                                                           (S
                                                                                                              Z)))))))))))
                                                                           ':-> ('Just
                                                                                   ('MkPiece
                                                                                      'Black
                                                                                      'Knight
                                                                                      ('Info
                                                                                         Z
                                                                                         ('At
                                                                                            'G
                                                                                            (S
                                                                                               (S
                                                                                                  (S
                                                                                                     (S
                                                                                                        (S
                                                                                                           (S
                                                                                                              (S
                                                                                                                 (S
                                                                                                                    Z)))))))))))
                                                                                 ':-> ('Just
                                                                                         ('MkPiece
                                                                                            'Black
                                                                                            'Rook
                                                                                            ('Info
                                                                                               Z
                                                                                               ('At
                                                                                                  'H
                                                                                                  (S
                                                                                                     (S
                                                                                                        (S
                                                                                                           (S
                                                                                                              (S
                                                                                                                 (S
                                                                                                                    (S
                                                                                                                       (S
                                                                                                                          Z)))))))))))
                                                                                       ':-> 'VEnd))))))))
                                            ':-> 'VEnd)))))))