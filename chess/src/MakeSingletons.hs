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

a1 :: Proxy ('At A Nat1)
a1 = Proxy @('At A Nat1)
a2 :: Proxy ('At A Nat2)
a2 = Proxy @('At A Nat2)
a3 :: Proxy ('At A Nat3)
a3 = Proxy @('At A Nat3)
a4 :: Proxy ('At A Nat4)
a4 = Proxy @('At A Nat4)
a5 :: Proxy ('At A Nat5)
a5 = Proxy @('At A Nat5)
a6 :: Proxy ('At A Nat6)
a6 = Proxy @('At A Nat6)
a7 :: Proxy ('At A Nat7)
a7 = Proxy @('At A Nat7)
a8 :: Proxy ('At A Nat8)
a8 = Proxy @('At A Nat8)

b1 :: Proxy ('At B Nat1)
b1 = Proxy @('At B Nat1)
b2 :: Proxy ('At B Nat2)
b2 = Proxy @('At B Nat2)
b3 :: Proxy ('At B Nat3)
b3 = Proxy @('At B Nat3)
b4 :: Proxy ('At B Nat4)
b4 = Proxy @('At B Nat4)
b5 :: Proxy ('At B Nat5)
b5 = Proxy @('At B Nat5)
b6 :: Proxy ('At B Nat6)
b6 = Proxy @('At B Nat6)
b7 :: Proxy ('At B Nat7)
b7 = Proxy @('At B Nat7)
b8 :: Proxy ('At B Nat8)
b8 = Proxy @('At B Nat8)

c1 :: Proxy ('At C Nat1)
c1 = Proxy @('At C Nat1)
c2 :: Proxy ('At C Nat2)
c2 = Proxy @('At C Nat2)
c3 :: Proxy ('At C Nat3)
c3 = Proxy @('At C Nat3)
c4 :: Proxy ('At C Nat4)
c4 = Proxy @('At C Nat4)
c5 :: Proxy ('At C Nat5)
c5 = Proxy @('At C Nat5)
c6 :: Proxy ('At C Nat6)
c6 = Proxy @('At C Nat6)
c7 :: Proxy ('At C Nat7)
c7 = Proxy @('At C Nat7)
c8 :: Proxy ('At C Nat8)
c8 = Proxy @('At C Nat8)

d1 :: Proxy ('At D Nat1)
d1 = Proxy @('At D Nat1)
d2 :: Proxy ('At D Nat2)
d2 = Proxy @('At D Nat2)
d3 :: Proxy ('At D Nat3)
d3 = Proxy @('At D Nat3)
d4 :: Proxy ('At D Nat4)
d4 = Proxy @('At D Nat4)
d5 :: Proxy ('At D Nat5)
d5 = Proxy @('At D Nat5)
d6 :: Proxy ('At D Nat6)
d6 = Proxy @('At D Nat6)
d7 :: Proxy ('At D Nat7)
d7 = Proxy @('At D Nat7)
d8 :: Proxy ('At D Nat8)
d8 = Proxy @('At D Nat8)

e1 :: Proxy ('At E Nat1)
e1 = Proxy @('At E Nat1)
e2 :: Proxy ('At E Nat2)
e2 = Proxy @('At E Nat2)
e3 :: Proxy ('At E Nat3)
e3 = Proxy @('At E Nat3)
e4 :: Proxy ('At E Nat4)
e4 = Proxy @('At E Nat4)
e5 :: Proxy ('At E Nat5)
e5 = Proxy @('At E Nat5)
e6 :: Proxy ('At E Nat6)
e6 = Proxy @('At E Nat6)
e7 :: Proxy ('At E Nat7)
e7 = Proxy @('At E Nat7)
e8 :: Proxy ('At E Nat8)
e8 = Proxy @('At E Nat8)

f1 :: Proxy ('At F Nat1)
f1 = Proxy @('At F Nat1)
f2 :: Proxy ('At F Nat2)
f2 = Proxy @('At F Nat2)
f3 :: Proxy ('At F Nat3)
f3 = Proxy @('At F Nat3)
f4 :: Proxy ('At F Nat4)
f4 = Proxy @('At F Nat4)
f5 :: Proxy ('At F Nat5)
f5 = Proxy @('At F Nat5)
f6 :: Proxy ('At F Nat6)
f6 = Proxy @('At F Nat6)
f7 :: Proxy ('At F Nat7)
f7 = Proxy @('At F Nat7)
f8 :: Proxy ('At F Nat8)
f8 = Proxy @('At F Nat8)

g1 :: Proxy ('At G Nat1)
g1 = Proxy @('At G Nat1)
g2 :: Proxy ('At G Nat2)
g2 = Proxy @('At G Nat2)
g3 :: Proxy ('At G Nat3)
g3 = Proxy @('At G Nat3)
g4 :: Proxy ('At G Nat4)
g4 = Proxy @('At G Nat4)
g5 :: Proxy ('At G Nat5)
g5 = Proxy @('At G Nat5)
g6 :: Proxy ('At G Nat6)
g6 = Proxy @('At G Nat6)
g7 :: Proxy ('At G Nat7)
g7 = Proxy @('At G Nat7)
g8 :: Proxy ('At G Nat8)
g8 = Proxy @('At G Nat8)

h1 :: Proxy ('At H Nat1)
h1 = Proxy @('At H Nat1)
h2 :: Proxy ('At H Nat2)
h2 = Proxy @('At H Nat2)
h3 :: Proxy ('At H Nat3)
h3 = Proxy @('At H Nat3)
h4 :: Proxy ('At H Nat4)
h4 = Proxy @('At H Nat4)
h5 :: Proxy ('At H Nat5)
h5 = Proxy @('At H Nat5)
h6 :: Proxy ('At H Nat6)
h6 = Proxy @('At H Nat6)
h7 :: Proxy ('At H Nat7)
h7 = Proxy @('At H Nat7)
h8 :: Proxy ('At H Nat8)
h8 = Proxy @('At H Nat8)

_Knight :: Proxy ('Knight)
_Knight = Proxy @Knight
_N = _Knight

_Rook :: Proxy ('Rook)
_Rook = Proxy @Rook
_R = _Rook

_Bishop :: Proxy ('Bishop)
_Bishop = Proxy @Bishop
_B = _Bishop

_Queen :: Proxy ('Queen)
_Queen = Proxy @Queen
_Q = _Queen

_Pawn :: Proxy ('Pawn)
_Pawn = Proxy @Pawn
_P = _Pawn

_King :: Proxy ('King)
_King = Proxy @King
_K = _King

white :: Proxy ('White)
white = Proxy @White
_Wh = white

black :: Proxy ('Black)
black = Proxy @Black
_Bl = black
