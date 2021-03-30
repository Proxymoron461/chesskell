module FlatBuilders where

import MakeSingletons
import ChessTypes
import Data.Singletons
import Data.Singletons.Prelude.Bool
import Data.Type.Nat hiding (SNat(..))
import Lib
import Vec
import FirstClassFunctions
import qualified GHC.TypeLits as TL

-- A continuation has form (t -> m)
type Spec t = forall m. (t -> m) -> m

type Conv s t = s -> Spec t

type Term t r = t -> r

----------------------------------------------------------------------------------------------------

-- Spec Int has type (Int -> r) -> r, for some type r
-- In other words, if you create a Spec Piece, all you need next is a function that takes a piece

-- Starts as black, because the first move needs to be white
-- type Spec t = forall m. (t -> m) -> m
chess :: Spec (Proxy StartDec)
chess cont = cont (Proxy @StartDec)

data MoveArgs where
    MA :: BoardDecorator -> Position -> PieceName -> Position -> MoveArgs

pawn :: Proxy (b :: BoardDecorator) -> Proxy (fromPos :: Position) -> Spec (Proxy (MA b fromPos 'Pawn))
pawn (dec :: Proxy b) (from :: Proxy (fromPos :: Position)) cont = cont (Proxy @(MA b fromPos Pawn))

rook :: Proxy (b :: BoardDecorator) -> Proxy (fromPos :: Position) -> Spec (Proxy (MA b fromPos 'Rook))
rook (dec :: Proxy b) (from :: Proxy (fromPos :: Position)) cont = cont (Proxy @(MA b fromPos Rook))

bishop :: Proxy (b :: BoardDecorator) -> Proxy (fromPos :: Position) -> Spec (Proxy (MA b fromPos 'Bishop))
bishop (dec :: Proxy b) (from :: Proxy (fromPos :: Position)) cont = cont (Proxy @(MA b fromPos Bishop))

queen :: Proxy (b :: BoardDecorator) -> Proxy (fromPos :: Position) -> Spec (Proxy (MA b fromPos 'Queen))
queen (dec :: Proxy b) (from :: Proxy (fromPos :: Position)) cont = cont (Proxy @(MA b fromPos Queen))

knight :: Proxy (b :: BoardDecorator) -> Proxy (fromPos :: Position) -> Spec (Proxy (MA b fromPos 'Knight))
knight (dec :: Proxy b) (from :: Proxy (fromPos :: Position)) cont = cont (Proxy @(MA b fromPos Knight))

king :: Proxy (b :: BoardDecorator) -> Proxy (fromPos :: Position) -> Spec (Proxy (MA b fromPos 'King))
king (dec :: Proxy b) (from :: Proxy (fromPos :: Position)) cont = cont (Proxy @(MA b fromPos King))

to :: Proxy (MA (b :: BoardDecorator) (fromPos :: Position) (n :: PieceName)) -> Proxy (toPos :: Position)
      -> Spec (Proxy (Eval (IfPieceThenMove n fromPos toPos b)))
to (args :: Proxy (MA (b :: BoardDecorator) (fromPos :: Position) (n :: PieceName))) (to' :: Proxy toPos)  cont
    = cont (Proxy @(Eval (IfPieceThenMove n fromPos toPos b)))

-- Shorthands
p :: Proxy (b :: BoardDecorator) -> Proxy (toPos :: Position) -> Spec (Proxy (MoveTo Pawn toPos b))
p (dec :: Proxy b) (to :: Proxy toPos) cont = cont (Proxy @(MoveTo Pawn toPos b))
_5 = p

n :: Proxy (b :: BoardDecorator) -> Proxy (toPos :: Position) -> Spec (Proxy (MoveTo Knight toPos b))
n (dec :: Proxy b) (to :: Proxy toPos) cont = cont (Proxy @(MoveTo Knight toPos b))
_4 = n

r :: Proxy (b :: BoardDecorator) -> Proxy (toPos :: Position) -> Spec (Proxy (MoveTo Rook toPos b))
r (dec :: Proxy b) (to :: Proxy toPos) cont = cont (Proxy @(MoveTo Rook toPos b))
_2 = r

b :: Proxy (b :: BoardDecorator) -> Proxy (toPos :: Position) -> Spec (Proxy (MoveTo Bishop toPos b))
b (dec :: Proxy b) (to :: Proxy toPos) cont = cont (Proxy @(MoveTo Bishop toPos b))
_3 = b

q :: Proxy (b :: BoardDecorator) -> Proxy (toPos :: Position) -> Spec (Proxy (MoveTo Queen toPos b))
q (dec :: Proxy b) (to :: Proxy toPos) cont = cont (Proxy @(MoveTo Queen toPos b))
_1 = q

k :: Proxy (b :: BoardDecorator) -> Proxy (toPos :: Position) -> Spec (Proxy (MoveTo King toPos b))
k (dec :: Proxy b) (to :: Proxy toPos) cont = cont (Proxy @(MoveTo King toPos b))
_6 = k

type family CastleMove (l :: Bool) (b :: BoardDecorator) :: BoardDecorator where
    CastleMove True  (Dec board Black pos kings n) = MoveTo King (At G Nat1) (Dec board Black pos kings n)
    CastleMove True  (Dec board White pos kings n) = MoveTo King (At G Nat8) (Dec board White pos kings n)
    CastleMove False (Dec board Black pos kings n) = MoveTo King (At C Nat1) (Dec board Black pos kings n)
    CastleMove False (Dec board White pos kings n) = MoveTo King (At C Nat8) (Dec board White pos kings n)

o_o :: Proxy (b :: BoardDecorator) -> Spec (Proxy (CastleMove True b))
o_o (dec :: Proxy b) cont = cont (Proxy @(CastleMove True b))
o_o_o :: Proxy (b :: BoardDecorator) -> Spec (Proxy (CastleMove False b))
o_o_o (dec :: Proxy b) cont = cont (Proxy @(CastleMove False b))

-- Even though it's a Proxy TypeError, it will split out errors just fine!
end :: Term (Proxy (b :: BoardDecorator)) (Proxy (b :: BoardDecorator))
end = id

becomes :: Proxy (b :: BoardDecorator) -> Proxy (name :: PieceName)
           -> Spec (Proxy (PromotePieceTo' name (GetLastPosition b) b))
becomes (dec :: Proxy b) (n :: Proxy name) cont = cont (Proxy @(PromotePieceTo' name (GetLastPosition b) b))

promoteTo :: Proxy (MA (b :: BoardDecorator) (fromPos :: Position) (n :: PieceName)) -> Proxy (promoteTo :: PieceName) -> Proxy (toPos :: Position)
      -> Spec (Proxy (Eval (PromotePawnMove fromPos toPos promoteTo b)))
promoteTo (args :: Proxy (MA (b :: BoardDecorator) (fromPos :: Position) (n :: PieceName))) (pro :: Proxy (promoteTo :: PieceName)) (to' :: Proxy toPos)  cont
    = cont (Proxy @(Eval (PromotePawnMove fromPos toPos promoteTo b)))

-- data SetPieceAtDec :: Piece -> BoardDecorator -> Position -> Exp BoardDecorator
data CreateArgs where
    CA :: BoardDecorator -> Team -> PieceName -> Position -> CreateArgs

create :: Spec (Proxy JustKingsDec)
create cont = cont (Proxy @JustKingsDec)

put :: Proxy (b :: BoardDecorator) -> Proxy (team :: Team) -> Proxy (name :: PieceName) -> Spec (Proxy (CA b team name))
put (dec :: Proxy b) (t :: Proxy team) (p :: Proxy name) cont = cont (Proxy @(CA b team name))

at :: Proxy (CA (b :: BoardDecorator) (team :: Team) (name :: PieceName)) -> Proxy (toPos :: Position)
      -> Spec (Proxy (Eval (SetPieceAtDec (MkPiece team name (Info Z toPos)) b toPos)))
at (dec :: Proxy (CA b team name)) (p :: Proxy toPos) cont
    = cont (Proxy @(Eval (SetPieceAtDec (MkPiece team name (Info Z toPos)) b toPos)))

lastTeam :: Proxy (b :: BoardDecorator) -> Proxy (team :: Team) -> Spec (Proxy (SetLastTeam b team))
lastTeam (dec :: Proxy b) (t :: Proxy team) cont
    = cont (Proxy @(SetLastTeam b team))
lastteam = lastTeam

lastMoved :: Proxy (b :: BoardDecorator) -> Proxy (pos :: Position) -> Spec (Proxy (SetLastPosition pos b))
lastMoved (dec :: Proxy b) (t :: Proxy (pos :: Position)) cont
    = cont (Proxy @(SetLastPosition pos b))

lastmoved = lastMoved

startMoves :: Conv (Proxy (b :: BoardDecorator)) (Proxy (b :: BoardDecorator))
startMoves (dec :: Proxy b) cont = cont dec
startmoves = startMoves

-- TODO: Introduce a bunch of different EDSL endings that you need!
endGetBoard :: Term (Proxy (a :: BoardDecorator)) (Proxy (GetBoard a))
endGetBoard (Proxy :: Proxy (b :: BoardDecorator)) = Proxy @(GetBoard b)

-- TODO: Code should deal with:
    -- Checkmate (compile or don't compile?)
    -- Draw (can only move into check)
    -- Trapped pieces

data Fen (n :: Nat) where
    FF  :: Fen Nat0
    F1  :: Fen n -> Fen (S n)
    F2  :: Fen n -> Fen (S (S n))
    F3  :: Fen n -> Fen (S (S (S n)))
    F4  :: Fen n -> Fen (S (S (S (S n))))
    F5  :: Fen n -> Fen (S (S (S (S (S n)))))
    F6  :: Fen n -> Fen (S (S (S (S (S (S n))))))
    F7  :: Fen n -> Fen (S (S (S (S (S (S (S n)))))))
    F8  :: Fen Nat8
    Pw  :: Fen n -> Fen (S n)
    Nw  :: Fen n -> Fen (S n)
    Qw  :: Fen n -> Fen (S n)
    Kw  :: Fen n -> Fen (S n)
    Bw  :: Fen n -> Fen (S n)
    Rw  :: Fen n -> Fen (S n)
    Pb  :: Fen n -> Fen (S n)
    Nb  :: Fen n -> Fen (S n)
    Qb  :: Fen n -> Fen (S n)
    Kb  :: Fen n -> Fen (S n)
    Bb  :: Fen n -> Fen (S n)
    Rb  :: Fen n -> Fen (S n)

fn0 :: Term (Proxy (b :: Fen n)) (Proxy (b :: Fen n))
fn0 = id

fn1 :: (Proxy (b :: Fen n)) -> Spec (Proxy (F1 b))
fn1 (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(F1 b))

fn2 :: (Proxy (b :: Fen n)) -> Spec (Proxy (F2 b))
fn2 (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(F2 b))

fn3 :: (Proxy (b :: Fen n)) -> Spec (Proxy (F3 b))
fn3 (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(F3 b))

fn4 :: (Proxy (b :: Fen n)) -> Spec (Proxy (F4 b))
fn4 (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(F4 b))

fn5 :: (Proxy (b :: Fen n)) -> Spec (Proxy (F5 b))
fn5 (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(F5 b))

fn6 :: (Proxy (b :: Fen n)) -> Spec (Proxy (F6 b))
fn6 (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(F6 b))

fn7 :: (Proxy (b :: Fen n)) -> Spec (Proxy (F7 b))
fn7 (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(F7 b))

ff :: Spec (Proxy FF)
ff cont = cont (Proxy @FF)

fn8 :: Proxy F8
fn8 = Proxy @(F8)

wP :: (Proxy (b :: Fen n)) -> Spec (Proxy (Pw b))
wP (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Pw b))

wQ :: (Proxy (b :: Fen n)) -> Spec (Proxy (Qw b))
wQ (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Qw b))

wN :: (Proxy (b :: Fen n)) -> Spec (Proxy (Nw b))
wN (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Nw b))

wK :: (Proxy (b :: Fen n)) -> Spec (Proxy (Kw b))
wK (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Kw b))

wB :: (Proxy (b :: Fen n)) -> Spec (Proxy (Bw b))
wB (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Bw b))

wR :: (Proxy (b :: Fen n)) -> Spec (Proxy (Rw b))
wR (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Rw b))

bP :: (Proxy (b :: Fen n)) -> Spec (Proxy (Pb b))
bP (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Pb b))

bQ :: (Proxy (b :: Fen n)) -> Spec (Proxy (Qb b))
bQ (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Qb b))

bN :: (Proxy (b :: Fen n)) -> Spec (Proxy (Nb b))
bN (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Nb b))

bK :: (Proxy (b :: Fen n)) -> Spec (Proxy (Kb b))
bK (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Kb b))

bB :: (Proxy (b :: Fen n)) -> Spec (Proxy (Bb b))
bB (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Bb b))

bR :: (Proxy (b :: Fen n)) -> Spec (Proxy (Rb b))
bR (Proxy :: Proxy (b :: Fen n)) cont = cont (Proxy @(Rb b))

fen1 :: (Proxy (b :: BoardDecorator)) -> (Proxy (f :: Fen Eight)) -> Spec (Proxy (SetRowDec' b Nat1 (FenToRow f Nat1)))
fen1 (Proxy :: Proxy (b :: BoardDecorator)) (Proxy :: Proxy (f :: Fen n)) cont = cont (Proxy @(SetRowDec' b Nat1 (FenToRow f Nat1)))

fen2 :: (Proxy (b :: BoardDecorator)) -> (Proxy (f :: Fen Eight)) -> Spec (Proxy (SetRowDec' b Nat2 (FenToRow f Nat2)))
fen2 (Proxy :: Proxy (b :: BoardDecorator)) (Proxy :: Proxy (f :: Fen n)) cont = cont (Proxy @(SetRowDec' b Nat2 (FenToRow f Nat2)))

fen3 :: (Proxy (b :: BoardDecorator)) -> (Proxy (f :: Fen Eight)) -> Spec (Proxy (SetRowDec' b Nat3 (FenToRow f Nat3)))
fen3 (Proxy :: Proxy (b :: BoardDecorator)) (Proxy :: Proxy (f :: Fen n)) cont = cont (Proxy @(SetRowDec' b Nat3 (FenToRow f Nat3)))

fen4 :: (Proxy (b :: BoardDecorator)) -> (Proxy (f :: Fen Eight)) -> Spec (Proxy (SetRowDec' b Nat4 (FenToRow f Nat4)))
fen4 (Proxy :: Proxy (b :: BoardDecorator)) (Proxy :: Proxy (f :: Fen n)) cont = cont (Proxy @(SetRowDec' b Nat4 (FenToRow f Nat4)))

fen5 :: (Proxy (b :: BoardDecorator)) -> (Proxy (f :: Fen Eight)) -> Spec (Proxy (SetRowDec' b Nat5 (FenToRow f Nat5)))
fen5 (Proxy :: Proxy (b :: BoardDecorator)) (Proxy :: Proxy (f :: Fen n)) cont = cont (Proxy @(SetRowDec' b Nat5 (FenToRow f Nat5)))

fen6 :: (Proxy (b :: BoardDecorator)) -> (Proxy (f :: Fen Eight)) -> Spec (Proxy (SetRowDec' b Nat6 (FenToRow f Nat6)))
fen6 (Proxy :: Proxy (b :: BoardDecorator)) (Proxy :: Proxy (f :: Fen n)) cont = cont (Proxy @(SetRowDec' b Nat6 (FenToRow f Nat6)))

fen7 :: (Proxy (b :: BoardDecorator)) -> (Proxy (f :: Fen Eight)) -> Spec (Proxy (SetRowDec' b Nat7 (FenToRow f Nat7)))
fen7 (Proxy :: Proxy (b :: BoardDecorator)) (Proxy :: Proxy (f :: Fen n)) cont = cont (Proxy @(SetRowDec' b Nat7 (FenToRow f Nat7)))

fen8 :: (Proxy (b :: BoardDecorator)) -> (Proxy (f :: Fen Eight)) -> Spec (Proxy (SetRowDec' b Nat8 (FenToRow f Nat8)))
fen8 (Proxy :: Proxy (b :: BoardDecorator)) (Proxy :: Proxy (f :: Fen n)) cont = cont (Proxy @(SetRowDec' b Nat8 (FenToRow f Nat8)))

type family FenToRow (f :: Fen Eight) (r :: Nat) :: Row where
    FenToRow x r = FenHelper (FenReverse' x) r A

type family FenHelper (f :: Fen n) (r :: Nat) (c :: Column) :: Vec n (Maybe Piece) where
    FenHelper FF       row col = VEnd
    FenHelper F8       row col = EmptyRow
    FenHelper (F1 fen) row col = Nothing :-> FenHelper fen row (R col)
    FenHelper (F2 fen) row col = Nothing :-> Nothing :-> FenHelper fen row (R (R col))
    FenHelper (F3 fen) row col = Nothing :-> Nothing :-> Nothing :-> FenHelper fen row (R (R (R col)))
    FenHelper (F4 fen) row col = Nothing :-> Nothing :-> Nothing :-> Nothing :-> FenHelper fen row (R (R (R (R col))))
    FenHelper (F5 fen) row col = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> FenHelper fen row (R (R (R (R (R col)))))
    FenHelper (F6 fen) row col = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> FenHelper fen row (R (R (R (R (R (R col))))))
    FenHelper (F7 fen) row col = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> FenHelper fen row (R (R (R (R (R (R (R col)))))))
    FenHelper (Pw fen) row col = Just (MkPiece White Pawn (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Nw fen) row col = Just (MkPiece White Knight (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Qw fen) row col = Just (MkPiece White Queen (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Kw fen) row col = Just (MkPiece White King (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Bw fen) row col = Just (MkPiece White Bishop (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Rw fen) row col = Just (MkPiece White Rook (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Pb fen) row col = Just (MkPiece Black Pawn (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Nb fen) row col = Just (MkPiece Black Knight (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Qb fen) row col = Just (MkPiece Black Queen (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Kb fen) row col = Just (MkPiece Black King (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Bb fen) row col = Just (MkPiece Black Bishop (Info Z (At col row))) :-> FenHelper fen row (R col)
    FenHelper (Rb fen) row col = Just (MkPiece Black Rook (Info Z (At col row))) :-> FenHelper fen row (R col)

type family FenReverse' (f :: Fen n) :: Fen n where
    FenReverse' FF = FF
    FenReverse' F8 = F8
    FenReverse' (F1 fen) = FenR (F1 FF) fen
    FenReverse' (F2 fen) = FenR (F2 FF) fen
    FenReverse' (F3 fen) = FenR (F3 FF) fen
    FenReverse' (F4 fen) = FenR (F4 FF) fen
    FenReverse' (F5 fen) = FenR (F5 FF) fen
    FenReverse' (F6 fen) = FenR (F6 FF) fen
    FenReverse' (F7 fen) = FenR (F7 FF) fen
    FenReverse' (Pw fen) = FenR (Pw FF) fen
    FenReverse' (Nw fen) = FenR (Nw FF) fen
    FenReverse' (Qw fen) = FenR (Qw FF) fen
    FenReverse' (Kw fen) = FenR (Kw FF) fen
    FenReverse' (Bw fen) = FenR (Bw FF) fen
    FenReverse' (Rw fen) = FenR (Rw FF) fen
    FenReverse' (Pb fen) = FenR (Pb FF) fen
    FenReverse' (Nb fen) = FenR (Nb FF) fen
    FenReverse' (Qb fen) = FenR (Qb FF) fen
    FenReverse' (Kb fen) = FenR (Kb FF) fen
    FenReverse' (Bb fen) = FenR (Bb FF) fen
    FenReverse' (Rb fen) = FenR (Rb FF) fen

type family NAdd (x :: Nat) (y :: Nat) :: Nat where
    NAdd Z m = m
    NAdd (S n) m = S (NAdd n m)

type family FenR (x :: Fen n) (y :: Fen m) :: Fen q where
    FenR sofar FF = sofar
    FenR sofar F8 = TL.TypeError (TL.Text "Should not be trying to reverse F8!")
    FenR sofar (F1 fen) = FenR (F1 sofar) fen
    FenR sofar (F2 fen) = FenR (F2 sofar) fen
    FenR sofar (F3 fen) = FenR (F3 sofar) fen
    FenR sofar (F4 fen) = FenR (F4 sofar) fen
    FenR sofar (F5 fen) = FenR (F5 sofar) fen
    FenR sofar (F6 fen) = FenR (F6 sofar) fen
    FenR sofar (F7 fen) = FenR (F7 sofar) fen
    FenR sofar (Pw fen) = FenR (Pw sofar) fen
    FenR sofar (Nw fen) = FenR (Nw sofar) fen
    FenR sofar (Qw fen) = FenR (Qw sofar) fen
    FenR sofar (Kw fen) = FenR (Kw sofar) fen
    FenR sofar (Bw fen) = FenR (Bw sofar) fen
    FenR sofar (Rw fen) = FenR (Rw sofar) fen
    FenR sofar (Pb fen) = FenR (Pb sofar) fen
    FenR sofar (Nb fen) = FenR (Nb sofar) fen
    FenR sofar (Qb fen) = FenR (Qb sofar) fen
    FenR sofar (Kb fen) = FenR (Kb sofar) fen
    FenR sofar (Bb fen) = FenR (Bb sofar) fen
    FenR sofar (Rb fen) = FenR (Rb sofar) fen
