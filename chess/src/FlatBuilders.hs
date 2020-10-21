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

-- -- Flat builder example 1 from Mezzo
-- string :: String -> Spec String
-- string arg cont = cont arg

-- firstChar :: Conv String Char
-- firstChar str cont = cont (head str)

-- printAscii :: Term Char Int
-- printAscii = fromEnum

-- put :: Show a => Term a String
-- put = show

-- -- string "Hello" firstChar printAscii = 72
-- -- string "Hello" firstChar printChar = "'H'"

-- -- Flat builder example 2 from Mezzo
-- -- add :: Int -> ((Int -> Int) -> m) -> m
-- -- add 5 :: ((Int -> Int) -> m) -> m
-- -- add 5 _, _ :: (Int -> Int) -> m
-- add :: Int -> Spec (Int -> Int)
-- add arg cont = cont (arg +)

-- to :: (Int -> Int) -> Conv Int Int
-- to f x cont = cont (f x)

-- and' :: Conv a a
-- and' x cont = cont x

-- the :: Conv a a
-- the = and'

-- display :: Show a => Conv a String
-- display s cont = cont (show s)

-- result :: Term String String
-- result s = "Result: " ++ s

-- -- add 5 to 7 and' display the result = "Result: 12"

-- Want EDSL to have the form:
-- start white pawn A 4
-- black king B 6
-- white bishop C 7 end

-- So let's start my making it in non-CPS style!

-- move :: SPosition from -> SPosition to -> Proxy (b :: Maybe Board) -> Proxy (Eval (b >>= Move from to))
-- move (sFrom :: SPosition from) (sTo :: SPosition to) (pBoard :: Proxy (b :: Maybe Board))
--     = Proxy @(Eval (b >>= Move from to))

-- next :: (Proxy (b :: Maybe Board)) -> SPosition from -> SPosition to -> Proxy (Eval (b >>= Move from to))
-- next b f t = move f t b

-- start :: SPosition from -> SPosition to -> Proxy (Eval (Move from to StartBoard))
-- start f t = move f t (Proxy @('Just StartBoard))

-- move' :: SPosition from -> SPosition to -> Proxy '(b :: Maybe Board, t :: Team)
--          -> Proxy ( '(Eval (b >>= IfValidThenMove (HasTeam (Eval (OppositeTeam t))) from to), Eval (OppositeTeam t)) )
-- move' (sFrom :: SPosition from) (sTo :: SPosition to) (pBoard :: Proxy '(b :: Maybe Board, t :: Team))
--     = Proxy @( '(Eval (b >>= IfValidThenMove (HasTeam (Eval (OppositeTeam t))) from to), Eval (OppositeTeam t)) )

-- next' :: Proxy ( '(b :: Maybe Board, t :: Team) ) -> SPosition from -> SPosition to
--          -> Proxy ( '(Eval (b >>= IfValidThenMove (HasTeam (Eval (OppositeTeam t))) from to), Eval (OppositeTeam t)) )
-- next' b f t = move' f t b

-- -- Starts as black because White needs to make the next move
-- start' :: SPosition from -> SPosition to
--           -> Proxy '((Eval (IfValidThenMove (HasTeam White) from to StartBoard)), White)
-- start' f t = move' f t (Proxy @( '(Just StartBoard, Black) ))

sNat0 = SZ
sNat1 = SS sNat0
sNat2 = SS sNat1
sNat3 = SS sNat2
sNat4 = SS sNat3
sNat5 = SS sNat4
sNat6 = SS sNat5
sNat7 = SS sNat6

-- x = next (start (SAt SE sNat3) (SAt SG sNat3)) (SAt SD sNat3) (SAt SE sNat3)

-- x' = next' (start' (SAt SE sNat3) (SAt SG sNat3)) (SAt SD sNat3) (SAt SE sNat3)

-- -- Should not compile - White moves twice.
-- -- But there needs to be a better way of doing this.
-- y' = next' (start' (SAt SE sNat3) (SAt SG sNat3)) (SAt SG sNat3) (SAt SE sNat3)

----------------------------------------------------------------------------------------------------

-- Spec Int has type (Int -> r) -> r, for some type r
-- In other words, if you create a Spec Piece, all you need next is a function that takes a piece

-- move' :: SPosition from -> SPosition to -> Proxy '(b :: Maybe Board, t :: Team)
--       -> Proxy ( '(Eval (b >>= IfValidThenMove (HasTeam (Eval (OppositeTeam t))) from to), Eval (OppositeTeam t)) )

-- Starts as black, because the first move needs to be white
chess :: Spec (Proxy (b :: BoardDecorator))
chess cont = cont (Proxy @(Dec StartBoard Black))

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
to :: SPosition toPos -> Proxy (MA (b :: BoardDecorator) (fromPos :: Position) (n :: PieceName))
      -> Spec (Proxy (Eval (MoveWithStateCheck (IsPiece n) fromPos toPos b)))
to (to' :: SPosition toPos) (args :: Proxy (MA (b :: BoardDecorator) (fromPos :: Position) (n :: PieceName))) cont
    = cont (Proxy @(Eval (MoveWithStateCheck (IsPiece n) fromPos toPos b)))

end :: Term (Proxy (b :: BoardDecorator)) (Proxy (b :: BoardDecorator))
end = id

-- TODO: Generate all the positions, with names like a4, h8, etc.

-- Having a go and seeing if it compiles!
x = chess pawn (SAt SA sNat2) to (SAt SA sNat4) pawn (SAt SA sNat7) to (SAt SA sNat6) end