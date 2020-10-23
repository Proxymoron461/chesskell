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

sNat0 = SZ
sNat1 = SS sNat0
sNat2 = SS sNat1
sNat3 = SS sNat2
sNat4 = SS sNat3
sNat5 = SS sNat4
sNat6 = SS sNat5
sNat7 = SS sNat6

----------------------------------------------------------------------------------------------------

-- Spec Int has type (Int -> r) -> r, for some type r
-- In other words, if you create a Spec Piece, all you need next is a function that takes a piece

-- Starts as black, because the first move needs to be white
-- type Spec t = forall m. (t -> m) -> m
chess :: Spec (Proxy (Dec StartBoard Black))
chess cont = cont (Proxy @(Dec StartBoard Black))
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
      -> Spec (Proxy (Eval (MoveWithStateCheck (IsPiece n) fromPos toPos b)))
to (args :: Proxy (MA (b :: BoardDecorator) (fromPos :: Position) (n :: PieceName))) (to' :: SPosition toPos)  cont
    = cont (Proxy @(Eval (MoveWithStateCheck (IsPiece n) fromPos toPos b)))

end :: Term (Proxy (b :: BoardDecorator)) (Proxy (b :: BoardDecorator))
end = id

-- TODO: Generate all the positions, with names like a4, h8, etc.

-- Having a go and seeing if it compiles!
x = chess pawn (SAt SA sNat2) to (SAt SA sNat4) pawn (SAt SA sNat7) to (SAt SA sNat6) end