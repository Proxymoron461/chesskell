module MakeSingletons where

import Data.Proxy(Proxy(..))
import ChessTypes
import FirstClassFunctions
import qualified GHC.TypeLits as TL
import Data.Singletons
import qualified Data.Singletons.TypeLits as TL
import Data.Singletons.TH
import Data.Singletons.Prelude.Bool
import Data.Type.Nat hiding (SNat(..))

-- Transform various types into singleton types (for use at the value-level)
$(genSingletons [''PieceName])
$(genSingletons [''Team])
$(genSingletons [''Nat])
$(genSingletons [''Column])
$(genSingletons [''Position])
$(genSingletons [''PieceInfo])
$(genSingletons [''Piece])

sNat0 = SZ
sNat1 = SS sNat0
sNat2 = SS sNat1
sNat3 = SS sNat2
sNat4 = SS sNat3
sNat5 = SS sNat4
sNat6 = SS sNat5
sNat7 = SS sNat6
sNat8 = SS sNat7

type SNat0 = 'SZ
type SNat1 = 'SS SNat0
type SNat2 = 'SS SNat1
type SNat3 = 'SS SNat2
type SNat4 = 'SS SNat3
type SNat5 = 'SS SNat4
type SNat6 = 'SS SNat5
type SNat7 = 'SS SNat6
type SNat8 = 'SS SNat7

a1 :: SPosition ('At A Nat1)
a1 = SAt SA sNat1
a2 :: SPosition ('At A Nat2)
a2 = SAt SA sNat2
a3 :: SPosition ('At A Nat3)
a3 = SAt SA sNat3
a4 :: SPosition ('At A Nat4)
a4 = SAt SA sNat4
a5 :: SPosition ('At A Nat5)
a5 = SAt SA sNat5
a6 :: SPosition ('At A Nat6)
a6 = SAt SA sNat6
a7 :: SPosition ('At A Nat7)
a7 = SAt SA sNat7
a8 :: SPosition ('At A Nat8)
a8 = SAt SA sNat8

b1 :: SPosition ('At B Nat1)
b1 = SAt SB sNat1
b2 :: SPosition ('At B Nat2)
b2 = SAt SB sNat2
b3 :: SPosition ('At B Nat3)
b3 = SAt SB sNat3
b4 :: SPosition ('At B Nat4)
b4 = SAt SB sNat4
b5 :: SPosition ('At B Nat5)
b5 = SAt SB sNat5
b6 :: SPosition ('At B Nat6)
b6 = SAt SB sNat6
b7 :: SPosition ('At B Nat7)
b7 = SAt SB sNat7
b8 :: SPosition ('At B Nat8)
b8 = SAt SB sNat8

c1 :: SPosition ('At C Nat1)
c1 = SAt SC sNat1
c2 :: SPosition ('At C Nat2)
c2 = SAt SC sNat2
c3 :: SPosition ('At C Nat3)
c3 = SAt SC sNat3
c4 :: SPosition ('At C Nat4)
c4 = SAt SC sNat4
c5 :: SPosition ('At C Nat5)
c5 = SAt SC sNat5
c6 :: SPosition ('At C Nat6)
c6 = SAt SC sNat6
c7 :: SPosition ('At C Nat7)
c7 = SAt SC sNat7
c8 :: SPosition ('At C Nat8)
c8 = SAt SC sNat8

d1 :: SPosition ('At D Nat1)
d1 = SAt SD sNat1
d2 :: SPosition ('At D Nat2)
d2 = SAt SD sNat2
d3 :: SPosition ('At D Nat3)
d3 = SAt SD sNat3
d4 :: SPosition ('At D Nat4)
d4 = SAt SD sNat4
d5 :: SPosition ('At D Nat5)
d5 = SAt SD sNat5
d6 :: SPosition ('At D Nat6)
d6 = SAt SD sNat6
d7 :: SPosition ('At D Nat7)
d7 = SAt SD sNat7
d8 :: SPosition ('At D Nat8)
d8 = SAt SD sNat8

e1 :: SPosition ('At E Nat1)
e1 = SAt SE sNat1
e2 :: SPosition ('At E Nat2)
e2 = SAt SE sNat2
e3 :: SPosition ('At E Nat3)
e3 = SAt SE sNat3
e4 :: SPosition ('At E Nat4)
e4 = SAt SE sNat4
e5 :: SPosition ('At E Nat5)
e5 = SAt SE sNat5
e6 :: SPosition ('At E Nat6)
e6 = SAt SE sNat6
e7 :: SPosition ('At E Nat7)
e7 = SAt SE sNat7
e8 :: SPosition ('At E Nat8)
e8 = SAt SE sNat8

f1 :: SPosition ('At F Nat1)
f1 = SAt SF sNat1
f2 :: SPosition ('At F Nat2)
f2 = SAt SF sNat2
f3 :: SPosition ('At F Nat3)
f3 = SAt SF sNat3
f4 :: SPosition ('At F Nat4)
f4 = SAt SF sNat4
f5 :: SPosition ('At F Nat5)
f5 = SAt SF sNat5
f6 :: SPosition ('At F Nat6)
f6 = SAt SF sNat6
f7 :: SPosition ('At F Nat7)
f7 = SAt SF sNat7
f8 :: SPosition ('At F Nat8)
f8 = SAt SF sNat8

g1 :: SPosition ('At G Nat1)
g1 = SAt SG sNat1
g2 :: SPosition ('At G Nat2)
g2 = SAt SG sNat2
g3 :: SPosition ('At G Nat3)
g3 = SAt SG sNat3
g4 :: SPosition ('At G Nat4)
g4 = SAt SG sNat4
g5 :: SPosition ('At G Nat5)
g5 = SAt SG sNat5
g6 :: SPosition ('At G Nat6)
g6 = SAt SG sNat6
g7 :: SPosition ('At G Nat7)
g7 = SAt SG sNat7
g8 :: SPosition ('At G Nat8)
g8 = SAt SG sNat8

h1 :: SPosition ('At H Nat1)
h1 = SAt SH sNat1
h2 :: SPosition ('At H Nat2)
h2 = SAt SH sNat2
h3 :: SPosition ('At H Nat3)
h3 = SAt SH sNat3
h4 :: SPosition ('At H Nat4)
h4 = SAt SH sNat4
h5 :: SPosition ('At H Nat5)
h5 = SAt SH sNat5
h6 :: SPosition ('At H Nat6)
h6 = SAt SH sNat6
h7 :: SPosition ('At H Nat7)
h7 = SAt SH sNat7
h8 :: SPosition ('At H Nat8)
h8 = SAt SH sNat8

_Knight :: SPieceName ('Knight)
_Knight = SKnight
_N = _Knight

_Rook :: SPieceName ('Rook)
_Rook = SRook
_R = _Rook

_Bishop :: SPieceName ('Bishop)
_Bishop = SBishop
_B = _Bishop

_Queen :: SPieceName ('Queen)
_Queen = SQueen
_Q = _Queen

_Pawn :: SPieceName ('Pawn)
_Pawn = SPawn
_P = _Pawn

_King :: SPieceName ('King)
_King = SKing
_K = _King

white :: STeam ('White)
white = SWhite
_Wh = white

black :: STeam ('Black)
black = SBlack
_Bl = black
