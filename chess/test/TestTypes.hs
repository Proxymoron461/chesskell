module TestTypes where

import qualified GHC.TypeLits as TL (Nat)

import Lib
import Vec
import FirstClassFunctions
import ChessTypes
import Data.Type.Nat hiding (SNat(..))

-- TEST TYPES
-- TODO: Remove these and replace with EDSL stuff
-- NOTE: These boards are upside-down - the first row is the last one visually
type TestPosition = At A Nat1  -- i.e. bottom left
type TestPiece    = MkPiece Black Pawn (Info Z TestPosition False)
type EmptyRow     = Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing
type TestBoard    = (Just TestPiece :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :<> EmptyRow

type TestWhitePawn = MkPiece White Pawn (Info Z (At A Nat2) False)
type TestWhitePawn2 = MkPiece White Pawn (Info Z (At A Nat7) False)
type TestWhitePawn3 = MkPiece White Pawn (Info Z (At B Nat3) False)
type TestBlackPawn = MkPiece Black Pawn (Info Z (At B Nat8) False)
type TestBoard2   = EmptyRow
                    :-> (Just TestWhitePawn :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> (Nothing :-> Just TestWhitePawn3 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> (Just TestWhitePawn2 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :<> (Nothing :-> Just TestBlackPawn :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)

type TestList = Eval (RangeBetween 0 10)

type EmptyBoard = EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :-> EmptyRow :<> EmptyRow

type TestInfo = Info Z (At A Nat1) False
type TestPieceList = '[MkPiece Black Pawn TestInfo, MkPiece White Pawn TestInfo, MkPiece White King TestInfo]