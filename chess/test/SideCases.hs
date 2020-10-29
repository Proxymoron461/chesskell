module SideCases where

import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

-- data IsSpaceVulnerableToEnPassant :: Team -> Board -> Position -> Exp Bool
-- Team is the ATTACKING TEAM
type WhitePawnFirst = FromJust' (Eval (Move (At A Nat2) (At A Nat4) StartDec))
enPassantTest1 :: True :~: Eval (IsSpaceVulnerableToEnPassant Black WhitePawnFirst (At A Nat4))
enPassantTest1 = Refl

enPassantTest2 :: False :~: Eval (IsSpaceVulnerableToEnPassant Black (FromJust' (Eval (Move (At A Nat2) (At A Nat3) StartDec))) (At A Nat3))
enPassantTest2 = Refl

type BlackPawnFirst = FromJust' (Eval (Move (At A Nat7) (At A Nat5) StartDec))
enPassantTest3 :: True :~: Eval (IsSpaceVulnerableToEnPassant White BlackPawnFirst (At A Nat5))
enPassantTest3 = Refl

enPassantTest4 :: False :~: Eval (IsSpaceVulnerableToEnPassant White (FromJust' (Eval (Move (At A Nat7) (At A Nat6) StartDec))) (At A Nat6))
enPassantTest4 = Refl

enPassantTest5 :: False :~: Eval (IsSpaceVulnerableToEnPassant Black (FromJust' (Eval (Move (At A Nat7) (At A Nat6) WhitePawnFirst))) (At A Nat4))
enPassantTest5 = Refl

enPassantTest6 :: False :~: Eval (IsSpaceVulnerableToEnPassant White (FromJust' (Eval (Move (At A Nat2) (At A Nat3) BlackPawnFirst))) (At A Nat5))
enPassantTest6 = Refl