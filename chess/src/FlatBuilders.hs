module FlatBuilders where

import MakeSingletons
import ChessTypes
import Data.Singletons
import Data.Singletons.Prelude.Bool
import Data.Type.Nat hiding (SNat(..))
import Lib
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
-- chess cont = cont (Proxy :: Proxy (Dec StartBoard Black))

data MoveArgs where
    MA :: BoardDecorator -> Position -> PieceName -> Position -> MoveArgs

pawn :: Proxy (b :: BoardDecorator) -> SPosition fromPos -> Spec (Proxy (MA b fromPos 'Pawn))
pawn (dec :: Proxy b) (from :: SPosition fromPos) cont = cont (Proxy @(MA b fromPos Pawn))

rook :: Proxy (b :: BoardDecorator) -> SPosition fromPos -> Spec (Proxy (MA b fromPos 'Rook))
rook (dec :: Proxy b) (from :: SPosition fromPos) cont = cont (Proxy @(MA b fromPos Rook))

bishop :: Proxy (b :: BoardDecorator) -> SPosition fromPos -> Spec (Proxy (MA b fromPos 'Bishop))
bishop (dec :: Proxy b) (from :: SPosition fromPos) cont = cont (Proxy @(MA b fromPos Bishop))

queen :: Proxy (b :: BoardDecorator) -> SPosition fromPos -> Spec (Proxy (MA b fromPos 'Queen))
queen (dec :: Proxy b) (from :: SPosition fromPos) cont = cont (Proxy @(MA b fromPos Queen))

knight :: Proxy (b :: BoardDecorator) -> SPosition fromPos -> Spec (Proxy (MA b fromPos 'Knight))
knight (dec :: Proxy b) (from :: SPosition fromPos) cont = cont (Proxy @(MA b fromPos Knight))

king :: Proxy (b :: BoardDecorator) -> SPosition fromPos -> Spec (Proxy (MA b fromPos 'King))
king (dec :: Proxy b) (from :: SPosition fromPos) cont = cont (Proxy @(MA b fromPos King))

-- data MoveWithStateCheck :: (Piece -> Exp Bool) -> Position -> Position -> BoardDecorator -> Exp BoardDecorator
to :: Proxy (MA (b :: BoardDecorator) (fromPos :: Position) (n :: PieceName)) -> SPosition toPos
      -> Spec (Proxy (Eval (MoveWithStateCheck n fromPos toPos b)))
to (args :: Proxy (MA (b :: BoardDecorator) (fromPos :: Position) (n :: PieceName))) (to' :: SPosition toPos)  cont
    = cont (Proxy @(Eval (MoveWithStateCheck n fromPos toPos b)))

-- TODO: "becomes", which allows promotion to whatever piece you say you should promote to

-- Even though it's a Proxy TypeError, it will split out errors just fine!
end :: Term (Proxy (b :: BoardDecorator)) (Proxy (b :: BoardDecorator))
end = id

becomes :: Proxy (b :: BoardDecorator) -> SPieceName name
           -> Spec (Proxy (PromotePieceTo name (GetLastPosition b) b))
becomes (dec :: Proxy b) (n :: SPieceName name) cont = cont (Proxy @(PromotePieceTo name (GetLastPosition b) b))

-- data SetPieceAtDec :: Piece -> BoardDecorator -> Position -> Exp BoardDecorator
data CreateArgs where
    CA :: BoardDecorator -> Team -> PieceName -> Position -> CreateArgs

create :: Spec (Proxy JustKingsDec)
create cont = cont (Proxy @JustKingsDec)

put :: Proxy (b :: BoardDecorator) -> STeam team -> SPieceName name -> Spec (Proxy (CA b team name))
put (dec :: Proxy b) (t :: STeam team) (p :: SPieceName name) cont = cont (Proxy @(CA b team name))

at :: Proxy (CA (b :: BoardDecorator) (team :: Team) (name :: PieceName)) -> SPosition toPos
      -> Spec (Proxy (Eval (SetPieceAtDec (MkPiece team name (Info Z toPos False)) b toPos)))
at (dec :: Proxy (CA b team name)) (p :: SPosition toPos) cont
    = cont (Proxy @(Eval (SetPieceAtDec (MkPiece team name (Info Z toPos False)) b toPos)))

lastTeam :: Proxy (b :: BoardDecorator) -> STeam team -> Spec (Proxy (SetLastTeam b team))
lastTeam (dec :: Proxy b) (t :: STeam team) cont
    = cont (Proxy @(SetLastTeam b team))
lastteam = lastTeam

lastMoved :: Proxy (b :: BoardDecorator) -> SPosition pos -> Spec (Proxy (SetLastPosition pos b))
lastMoved (dec :: Proxy b) (t :: SPosition pos) cont
    = cont (Proxy @(SetLastPosition pos b))

lastmoved = lastMoved

startMoves :: Conv (Proxy (b :: BoardDecorator)) (Proxy (b :: BoardDecorator))
startMoves (dec :: Proxy b) cont = cont dec
startmoves = startMoves

-- TODO: Introduce a bunch of different EDSL endings that you need!
endGetBoard :: Term (Proxy (a :: BoardDecorator)) (Proxy (GetBoard a))
endGetBoard (Proxy :: Proxy (b :: BoardDecorator)) = Proxy @(GetBoard b)

-- Having a go and seeing if it compiles!
x = chess
    pawn _a2 to _a4
    pawn _a7 to _a6
    end

-- TODO: Code should deal with:
    -- Checkmate (compile or don't compile?)
    -- Draw (can only move into check)
    -- Trapped pieces