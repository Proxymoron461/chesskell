module PawnTakeTests where

import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Test.HUnit.Lang (Assertion, assertFailure)
import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Chesskell
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

type WhiteTakePawn  = MkPiece White Pawn (Info Z (At A Nat2))
type WhiteTakePawn2 = MkPiece White Pawn (Info Z (At A Nat7))
type WhiteTakePawn3 = MkPiece White Pawn (Info (S Z) (At B Nat3))
type WhiteTakePawn4 = MkPiece White Pawn (Info Z (At B Nat6))
type BlackTakePawn  = MkPiece Black Pawn (Info Z (At A Nat8))
type BlackTakePawn2 = MkPiece Black Pawn (Info (S Z) (At C Nat2))
type BlackTakePawn3 = MkPiece Black Pawn (Info (S Z) (At A Nat3))
type BlackTakePawn4  = MkPiece Black Pawn (Info Z (At B Nat8))

type PawnTakeBoard = EmptyRow
                    :-> (Just WhiteTakePawn :-> Nothing :-> Just BlackTakePawn2 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> (Just BlackTakePawn3 :-> Just WhiteTakePawn3 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> (Nothing :-> Just WhiteTakePawn4 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> (Just WhiteTakePawn2 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Just (MkPiece White King (Info Z (At F Nat7))) :-> Nothing :<> Nothing)
                    :<> (Just BlackTakePawn :-> Just BlackTakePawn4 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Just (MkPiece Black King (Info Z (At H Nat8))))

type BlackLastPromDec = Dec PawnTakeBoard Black (At C Nat2) '(At F Nat7, At H Nat8) Nat2
type WhiteLastPromDec = Dec PawnTakeBoard White (At B Nat3) '(At F Nat7, At H Nat8) Nat2

-- blackCannotTakeInFront :: Proxy (a :: BoardDecorator)
-- blackCannotTakeInFront = Proxy @(Eval (Move (At A Nat8) (At A Nat7) WhiteLastPromDec))

-- blackCannotMoveOver :: Proxy (a :: BoardDecorator)
-- blackCannotMoveOver = Proxy @(Eval (Move (At A Nat8) (At A Nat6) WhiteLastPromDec))

-- blackCannotTakeTwoInFront :: Proxy (a :: BoardDecorator)
-- blackCannotTakeTwoInFront = Proxy @(Eval (Move (At B Nat8) (At B Nat6) WhiteLastPromDec))

-- whiteCannotTakeInFront :: Proxy (a :: BoardDecorator)
-- whiteCannotTakeInFront = Proxy @(Eval (Move (At A Nat2) (At A Nat3) BlackLastPromDec))

-- whiteCannotMoveOver :: Proxy (a :: BoardDecorator)
-- whiteCannotMoveOver = Proxy @(Eval (Move (At A Nat2) (At A Nat4) BlackLastPromDec))

-- pawnTakeTestSuite :: Test.Hspec.Spec
-- pawnTakeTestSuite = describe "Pawn Take Tests" $ do
--     it "1: A Black Pawn should not be able to take the piece directly below it" $
--         shouldNotTypecheck blackCannotTakeInFront
--     it "2: A Black Pawn cannot jump over a piece directly below it on its' first move" $
--         shouldNotTypecheck blackCannotMoveOver
--     it "3: A Black Pawn should not be able to take the piece two moves below it on its first move" $
--         shouldNotTypecheck blackCannotTakeTwoInFront
--     it "4: A White Pawn should not be able to take the piece directly above it" $
--         shouldNotTypecheck whiteCannotTakeInFront
--     it "5: A White Pawn cannot jump over a piece directly above it on its' first move" $
--         shouldNotTypecheck whiteCannotMoveOver
