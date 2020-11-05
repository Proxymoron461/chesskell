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

_a1 :: SPosition ('At A Nat1)
_a1 = SAt SA sNat1
_a2 :: SPosition ('At A Nat2)
_a2 = SAt SA sNat2
_a3 :: SPosition ('At A Nat3)
_a3 = SAt SA sNat3
_a4 :: SPosition ('At A Nat4)
_a4 = SAt SA sNat4
_a5 :: SPosition ('At A Nat5)
_a5 = SAt SA sNat5
_a6 :: SPosition ('At A Nat6)
_a6 = SAt SA sNat6
_a7 :: SPosition ('At A Nat7)
_a7 = SAt SA sNat7
_a8 :: SPosition ('At A Nat8)
_a8 = SAt SA sNat8

_b1 :: SPosition ('At B Nat1)
_b1 = SAt SB sNat1
_b2 :: SPosition ('At B Nat2)
_b2 = SAt SB sNat2
_b3 :: SPosition ('At B Nat3)
_b3 = SAt SB sNat3
_b4 :: SPosition ('At B Nat4)
_b4 = SAt SB sNat4
_b5 :: SPosition ('At B Nat5)
_b5 = SAt SB sNat5
_b6 :: SPosition ('At B Nat6)
_b6 = SAt SB sNat6
_b7 :: SPosition ('At B Nat7)
_b7 = SAt SB sNat7
_b8 :: SPosition ('At B Nat8)
_b8 = SAt SB sNat8

_c1 :: SPosition ('At C Nat1)
_c1 = SAt SC sNat1
_c2 :: SPosition ('At C Nat2)
_c2 = SAt SC sNat2
_c3 :: SPosition ('At C Nat3)
_c3 = SAt SC sNat3
_c4 :: SPosition ('At C Nat4)
_c4 = SAt SC sNat4
_c5 :: SPosition ('At C Nat5)
_c5 = SAt SC sNat5
_c6 :: SPosition ('At C Nat6)
_c6 = SAt SC sNat6
_c7 :: SPosition ('At C Nat7)
_c7 = SAt SC sNat7
_c8 :: SPosition ('At C Nat8)
_c8 = SAt SC sNat8

_d1 :: SPosition ('At D Nat1)
_d1 = SAt SD sNat1
_d2 :: SPosition ('At D Nat2)
_d2 = SAt SD sNat2
_d3 :: SPosition ('At D Nat3)
_d3 = SAt SD sNat3
_d4 :: SPosition ('At D Nat4)
_d4 = SAt SD sNat4
_d5 :: SPosition ('At D Nat5)
_d5 = SAt SD sNat5
_d6 :: SPosition ('At D Nat6)
_d6 = SAt SD sNat6
_d7 :: SPosition ('At D Nat7)
_d7 = SAt SD sNat7
_d8 :: SPosition ('At D Nat8)
_d8 = SAt SD sNat8

_e1 :: SPosition ('At E Nat1)
_e1 = SAt SE sNat1
_e2 :: SPosition ('At E Nat2)
_e2 = SAt SE sNat2
_e3 :: SPosition ('At E Nat3)
_e3 = SAt SE sNat3
_e4 :: SPosition ('At E Nat4)
_e4 = SAt SE sNat4
_e5 :: SPosition ('At E Nat5)
_e5 = SAt SE sNat5
_e6 :: SPosition ('At E Nat6)
_e6 = SAt SE sNat6
_e7 :: SPosition ('At E Nat7)
_e7 = SAt SE sNat7
_e8 :: SPosition ('At E Nat8)
_e8 = SAt SE sNat8

_f1 :: SPosition ('At F Nat1)
_f1 = SAt SF sNat1
_f2 :: SPosition ('At F Nat2)
_f2 = SAt SF sNat2
_f3 :: SPosition ('At F Nat3)
_f3 = SAt SF sNat3
_f4 :: SPosition ('At F Nat4)
_f4 = SAt SF sNat4
_f5 :: SPosition ('At F Nat5)
_f5 = SAt SF sNat5
_f6 :: SPosition ('At F Nat6)
_f6 = SAt SF sNat6
_f7 :: SPosition ('At F Nat7)
_f7 = SAt SF sNat7
_f8 :: SPosition ('At F Nat8)
_f8 = SAt SF sNat8

_g1 :: SPosition ('At G Nat1)
_g1 = SAt SG sNat1
_g2 :: SPosition ('At G Nat2)
_g2 = SAt SG sNat2
_g3 :: SPosition ('At G Nat3)
_g3 = SAt SG sNat3
_g4 :: SPosition ('At G Nat4)
_g4 = SAt SG sNat4
_g5 :: SPosition ('At G Nat5)
_g5 = SAt SG sNat5
_g6 :: SPosition ('At G Nat6)
_g6 = SAt SG sNat6
_g7 :: SPosition ('At G Nat7)
_g7 = SAt SG sNat7
_g8 :: SPosition ('At G Nat8)
_g8 = SAt SG sNat8

_h1 :: SPosition ('At H Nat1)
_h1 = SAt SH sNat1
_h2 :: SPosition ('At H Nat2)
_h2 = SAt SH sNat2
_h3 :: SPosition ('At H Nat3)
_h3 = SAt SH sNat3
_h4 :: SPosition ('At H Nat4)
_h4 = SAt SH sNat4
_h5 :: SPosition ('At H Nat5)
_h5 = SAt SH sNat5
_h6 :: SPosition ('At H Nat6)
_h6 = SAt SH sNat6
_h7 :: SPosition ('At H Nat7)
_h7 = SAt SH sNat7
_h8 :: SPosition ('At H Nat8)
_h8 = SAt SH sNat8
