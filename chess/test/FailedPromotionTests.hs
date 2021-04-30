module FailedPromotionTests where

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

type WhitePromPawn  = MkPiece White Pawn (Info Z (At A Nat2))
type WhitePromPawn2 = MkPiece White Pawn (Info Z (At A Nat7))
type WhitePromPawn3 = MkPiece White Pawn (Info (S Z) (At B Nat3))
type BlackPromPawn  = MkPiece Black Pawn (Info Z (At B Nat8))
type BlackPromPawn2 = MkPiece Black Pawn (Info (S Z) (At C Nat2))
type PromotionBoard = EmptyRow
                    :-> (Just WhitePromPawn :-> Nothing :-> Just BlackPromPawn2 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> (Nothing :-> Just WhitePromPawn3 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Nothing)
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> EmptyRow
                    :-> (Just WhitePromPawn2 :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Just (MkPiece White King (Info Z (At F Nat7))) :-> Nothing :<> Nothing)
                    :<> (Nothing :-> Just BlackPromPawn :-> Nothing :-> Nothing :-> Nothing :-> Nothing :-> Nothing :<> Just (MkPiece Black King (Info Z (At H Nat8))))

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
            shouldNotTypecheck whitePawnMustPromote
        it "2: A Black Pawn must promote when it is moving from row 2 to row 1" $
            shouldNotTypecheck blackPawnMustPromote
    describe "Disallowed Promotion Tests" $ do
        it "1: White Pieces cannot promote to Kings" $
            shouldNotTypecheck whiteKingPromotion
        it "2: Black Pieces cannot promote to Kings" $
            shouldNotTypecheck blackKingPromotion
        it "3: White Pieces cannot promote to Pawns" $
            shouldNotTypecheck whitePawnPromotion
        it "4: Black Pieces cannot promote to Pawns" $
            shouldNotTypecheck blackPawnPromotion
        