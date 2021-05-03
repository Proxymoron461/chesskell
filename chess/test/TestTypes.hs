module TestTypes where

import qualified GHC.TypeLits as TL (Nat)
import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, TypeError(..))
import Data.Type.Equality ((:~:)(..))
import Data.Proxy(Proxy(..))

import Chesskell
import Vec
import FirstClassFunctions
import ChessTypes
import FlatBuilders
import Ranges
import Data.Type.Nat hiding (SNat(..))
import Data.Proxy

-- Custom shouldTypeCheck assertion
shouldTypecheck :: NFData a => a -> Assertion
shouldTypecheck a = do
    result <- try (evaluate (force a))  -- Using Haskell’s do-notation
    case result of
        Right _ -> return ()  -- Test passes
        Left (TypeError msg) -> assertFailure ("Term didn’t compile.")

shouldTypeCheck :: NFData a => a -> Assertion
shouldTypeCheck = shouldTypecheck

shouldNotTypeCheck :: NFData a => a -> Assertion
shouldNotTypeCheck = shouldNotTypecheck

fromProxyFalse :: Proxy False -> False :~: False
fromProxyFalse (Proxy :: Proxy b) = Refl @(b)

fromProxyTrue :: Proxy True -> True :~: True
fromProxyTrue (Proxy :: Proxy b) = Refl @(b)

-- TEST TYPES
-- NOTE: These boards are upside-down - the first row is the bottom one visually
type TestPosition = At A Nat1  -- i.e. bottom left
type TestPiece    = MkPiece Black Pawn (Info Z TestPosition)
type TestBoard    = (Just TestPiece :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :<> EmptyRow

type TestDec = Dec TestBoard Black (At A Nat1) '(At A Z, At H Z) Nat1

type TestWhitePawn = MkPiece White Pawn (Info Z (At A Nat2))
type TestWhitePawn2 = MkPiece White Pawn (Info Z (At A Nat7))
type TestWhitePawn3 = MkPiece White Pawn (Info Z (At B Nat3))
type TestBlackPawn = MkPiece Black Pawn (Info Z (At B Nat8))
type TestBoard2   = EmptyRow
                    :-> (Just TestWhitePawn :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> (Nothing :-> Just TestWhitePawn3 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> (Just TestWhitePawn2 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :<> (Nothing :-> Just TestBlackPawn :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)

type TestDec2 = Dec TestBoard2 Black (At A Nat1) '(At A Z, At H Z) Nat1

type TestList = Eval (RangeBetween 0 10)

type TestInfo = Info Z (At A Nat1)
type TestPieceList = '[MkPiece Black Pawn TestInfo, MkPiece White Pawn TestInfo, MkPiece White King TestInfo]

type KingBoard = Eval (SetPiecesAt '[ '(MkPiece White King TestInfo, At A Nat1), '(MkPiece Black King TestInfo, At H Nat8) ] EmptyBoard)
type KingDec = Dec KingBoard Black (At H Nat8) '(At A Nat1, At H Nat8) Nat2
