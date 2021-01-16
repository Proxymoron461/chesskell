module FailedPromotionTests where

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes

import TestTypes

type WhitePromPawn  = MkPiece White Pawn (Info Z (At A Nat2) False)
type WhitePromPawn2 = MkPiece White Pawn (Info Z (At A Nat7) False)
type WhitePromPawn3 = MkPiece White Pawn (Info (S Z) (At B Nat3) False)
type BlackPromPawn  = MkPiece Black Pawn (Info Z (At B Nat8) False)
type BlackPromPawn2 = MkPiece Black Pawn (Info (S Z) (At C Nat2) False)
type PromotionBoard = EmptyRow
                    :-> (Just WhitePromPawn :-> Nothing :-> Just BlackPromPawn2 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> (Nothing :-> Just WhitePromPawn3 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> (Just WhitePromPawn2 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Just (MkPiece White King (Info Z (At F Nat7) False)) :-> Nothing :<> Nothing)
                    :<> (Nothing :-> Just BlackPromPawn :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Just (MkPiece Black King (Info Z (At H Nat8) False)))

type BlackLastPromDec = Dec PromotionBoard Black (At C Nat2) '(At F Nat7, At H Nat8) Nat2
type WhiteLastPromDec = Dec PromotionBoard White (At B Nat3) '(At F Nat7, At H Nat8) Nat2

whitePawnMustPromote :: Proxy (a :: BoardDecorator)
whitePawnMustPromote = Proxy @(Eval (Move (At A Nat7) (At A Nat8) BlackLastPromDec))

blackPawnMustPromote :: Proxy (a :: BoardDecorator)
blackPawnMustPromote = Proxy @(Eval (Move (At C Nat2) (At C Nat1) WhiteLastPromDec))

whiteKingPromotion :: Proxy (a :: BoardDecorator)
whiteKingPromotion = Proxy @(Eval (PromotePawnMove (At A Nat7) (At A Nat8) King BlackLastPromDec))

blackKingPromotion :: Proxy (a :: BoardDecorator)
blackKingPromotion = Proxy @(Eval (PromotePawnMove (At C Nat2) (At C Nat1) King WhiteLastPromDec))

whitePawnPromotion :: Proxy (a :: BoardDecorator)
whitePawnPromotion = Proxy @(Eval (PromotePawnMove (At A Nat7) (At A Nat8) Pawn BlackLastPromDec))

blackPawnPromotion :: Proxy (a :: BoardDecorator)
blackPawnPromotion = Proxy @(Eval (PromotePawnMove (At C Nat2) (At C Nat1) Pawn WhiteLastPromDec))

failedPromotionTestSuite = describe "Failed Promotion Tests" $ do
    describe "Must Promote Tests" $ do
        it "1: A White Pawn must promote when it is moving from row 7 to row 8" $
            shouldNotTypeCheck whitePawnMustPromote
        it "2: A Black Pawn must promote when it is moving from row 2 to row 1" $
            shouldNotTypeCheck blackPawnMustPromote
    describe "Disallowed Promotion Tests" $ do
        it "1: White Pieces cannot promote to Kings" $
            shouldNotTypeCheck whiteKingPromotion
        it "2: Black Pieces cannot promote to Kings" $
            shouldNotTypeCheck blackKingPromotion
        it "3: White Pieces cannot promote to Pawns" $
            shouldNotTypeCheck whitePawnPromotion
        it "4: Black Pieces cannot promote to Pawns" $
            shouldNotTypeCheck blackPawnPromotion
        