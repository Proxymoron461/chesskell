module FlatBuilders where

import MakeSingletons
import ChessTypes
import Data.Singletons
import Data.Singletons.Prelude.Bool
import Data.Type.Nat hiding (SNat(..))
import Lib
import FirstClassFunctions

-- A continuation has form (t -> m)
type Spec t = forall m. (t -> m) -> m

type Conv s t = s -> Spec t

type Term t r = t -> r

-- Flat builder example 1 from Mezzo
string :: String -> Spec String
string arg cont = cont arg

firstChar :: Conv String Char
firstChar str cont = cont (head str)

printAscii :: Term Char Int
printAscii = fromEnum

put :: Show a => Term a String
put = show

-- string "Hello" firstChar printAscii = 72
-- string "Hello" firstChar printChar = "'H'"

-- Flat builder example 2 from Mezzo
-- add :: Int -> ((Int -> Int) -> m) -> m
-- add 5 :: ((Int -> Int) -> m) -> m
-- add 5 _, _ :: (Int -> Int) -> m
add :: Int -> Spec (Int -> Int)
add arg cont = cont (arg +)

to :: (Int -> Int) -> Conv Int Int
to f x cont = cont (f x)

and' :: Conv a a
and' x cont = cont x

the :: Conv a a
the = and'

display :: Show a => Conv a String
display s cont = cont (show s)

result :: Term String String
result s = "Result: " ++ s

-- add 5 to 7 and' display the result = "Result: 12"

-- Want EDSL to have the form:
-- start white pawn A 4
-- black king B 6
-- white bishop C 7 end

-- So let's start my making it in non-CPS style!

type family PieceFromSing (s :: SPiece p) :: Piece where
    PieceFromSing ('SMkPiece team name info) = 'MkPiece (TeamFromSing team) (NameFromSing name) (InfoFromSing info)

type family TeamFromSing (s :: STeam t) :: Team where
    TeamFromSing SWhite = White
    TeamFromSing SBlack = Black

type family NameFromSing (s :: SPieceName t) :: PieceName where
    NameFromSing SPawn = Pawn
    NameFromSing SRook = Rook
    NameFromSing SBishop = Bishop
    NameFromSing SKnight = Knight
    NameFromSing SQueen = Queen
    NameFromSing SKing = King

type family InfoFromSing (s :: SPieceInfo t) :: PieceInfo where
    InfoFromSing (SInfo moves pos last) = 'Info (NatFromSing moves) (PosFromSing pos) (BoolFromSing last)

type family NatFromSing (s :: SNat n) :: Nat where
    NatFromSing SZ = Z
    NatFromSing (SS n) = S (NatFromSing n)

type family PosFromSing (s :: SPosition k) :: Position where
    PosFromSing (SAt col row) = 'At (ColFromSing col) (NatFromSing row)

type family ColFromSing (s :: SColumn k) :: Column where
    ColFromSing SA = A
    ColFromSing SB = B
    ColFromSing SC = C
    ColFromSing SD = D
    ColFromSing SE = E
    ColFromSing SF = F
    ColFromSing SG = G
    ColFromSing SH = H

type family BoolFromSing (s :: SBool b) :: Bool where
    BoolFromSing SFalse = False
    BoolFromSing STrue = True

move :: SPosition from -> SPosition to -> Proxy (b :: Maybe Board) -> Proxy (Eval (b >>= Move from to))
move (sFrom :: SPosition from) (sTo :: SPosition to) (pBoard :: Proxy (b :: Maybe Board))
    = Proxy @(Eval (b >>= Move from to))

next :: (Proxy (b :: Maybe Board)) -> SPosition from -> SPosition to -> Proxy (Eval (b >>= Move from to))
next b f t = move f t b

start :: SPosition from -> SPosition to -> Proxy (Eval (Move from to StartBoard))
start f t = move f t (Proxy @('Just StartBoard))

-- TODO: Make this the actual chess board start, with pieces in correct places
type StartBoard = MyTestBoard 

sNat0 = SZ
sNat1 = SS sNat0
sNat2 = SS sNat1
sNat3 = SS sNat2
sNat4 = SS sNat3
sNat5 = SS sNat4
sNat6 = SS sNat5
sNat7 = SS sNat6

x = start (SAt SE sNat3) (SAt SG sNat3)