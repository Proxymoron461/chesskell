module CastleTests where

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Lib
import Vec
import FirstClassFunctions
import ChessTypes
import FlatBuilders
import MakeSingletons

import TestTypes

castleToPosTest1 :: True :~: Eval ('[At C Nat1, At G Nat1] :=:=: Eval (CastleToPositions (At E Nat1)))
castleToPosTest1 = Refl

castleToPosTest2 :: True :~: Eval ('[At C Nat8, At G Nat8] :=:=: Eval (CastleToPositions (At E Nat8)))
castleToPosTest2 = Refl

-- data AnySpaceInCheck :: BoardDecorator -> [Position] -> Exp Bool
anySpaceInCheckTest1 :: False :~: Eval (AnySpaceInCheck JustKingsDec (SpacesBetweenInc (At A Nat5) (At H Nat5)))
anySpaceInCheckTest1 = Refl

anySpaceInCheckTest2 :: Proxy (b :: BoardDecorator) -> Proxy (Eval (AnySpaceInCheck b '[ At C Nat5 ]))
anySpaceInCheckTest2 (Proxy :: (Proxy (b :: BoardDecorator))) = Proxy @(Eval (AnySpaceInCheck b '[ At C Nat5 ]))
aSICT2Board = create put _Wh _P at _c4 lastTeam _Wh end

anySpaceInCheckTest3 :: Proxy (b :: BoardDecorator) -> Proxy (Eval (AnySpaceInCheck b '[ At B Nat5, At D Nat5 ]))
anySpaceInCheckTest3 (Proxy :: (Proxy (b :: BoardDecorator))) = Proxy @(Eval (AnySpaceInCheck b '[ At B Nat5, At D Nat5 ]))

allSpacesFreeTest1 :: True :~: Eval (AllSpacesFree JustKingsDec (SpacesBetweenInc (At A Nat5) (At H Nat5)))
allSpacesFreeTest1 = Refl

allSpacesFreeTest2 :: Proxy (b :: BoardDecorator) -> Proxy (Eval (AllSpacesFree b '[ At C Nat4, At D Nat4 ]))
allSpacesFreeTest2 (Proxy :: (Proxy (b :: BoardDecorator)))
   = Proxy @(Eval (AllSpacesFree b '[ At C Nat4, At D Nat4 ]))

allSpacesFreeTest3 :: False :~: Eval (AllSpacesFree JustKingsDec (SpacesBetweenInc (At E Nat1) (At E Nat8)))
allSpacesFreeTest3 = Refl

fromProxyFalse :: Proxy False -> False :~: False
fromProxyFalse (Proxy :: Proxy b) = Refl @(b)

fromProxyTrue :: Proxy True -> True :~: True
fromProxyTrue (Proxy :: Proxy b) = Refl @(b)

castleTestSuite = describe "Castle Tests" $ do
    describe "Castle Helper Function Tests" $ do
        describe "CastleToPosition Tests" $ do
            it "1: The White King, from its starting position, should be able to castle to C1 and G1" $
                shouldTypeCheck castleToPosTest1
            it "2: The Black King, from its starting position, should be able to castle to C8 and G8" $
                shouldTypeCheck castleToPosTest2
        describe "AnySpaceInCheck Tests" $ do
            it "1" $
                shouldTypeCheck anySpaceInCheckTest1
            it "2: A space above a White Pawn should NOT be in check" $
                shouldTypeCheck $ fromProxyFalse (anySpaceInCheckTest2 aSICT2Board)
            it "3: The spaces diagonally above a White Pawn should be in check" $
                shouldTypeCheck $ fromProxyTrue (anySpaceInCheckTest3 aSICT2Board)
        describe "AllSpacesFree Tests" $ do
            it "1: An empty row should be registered as free" $
               shouldTypeCheck allSpacesFreeTest1
            it "2: A position with a Pawn in should NOT be registered as free" $
               shouldTypeCheck $ fromProxyFalse (allSpacesFreeTest2 aSICT2Board)
            it "3: A position with a King in should NOT be registered as free" $
               shouldTypecheck allSpacesFreeTest3
    --     describe "HasKingMoved Tests" $ do
    --         it blah
    --     describe "HaveRooksMoved Tests" $ do
    --         it blah
    -- describe "CanCastle Tests" $ do
    --     it blah blah blah
    -- describe "Castling tests" $ do
    --     it blah blah blah

-------------------------------------------------------------------------------------------------------
-- MESSY DECLARATIONS
-------------------------------------------------------------------------------------------------------

-- onlyRooksAndKings = create
--                         put _Wh _R at _a1
--                         put _Wh _R at _h1
--                         put _Bl _R at _a8
--                         put _Bl _R at _h8
--                     end

-- Used above EDSL statement to get it in repl, then copied and pasted!
type OnlyRooksAndKings = 'Dec
          (('Just
              ('MkPiece
                 'White
                 'Rook
                 ('Info
                    Z ('At 'A (Nat1)) 'False))
            ':-> ('Nothing
                  ':-> ('Nothing
                        ':-> ('Nothing
                              ':-> ('Just
                                      ('MkPiece
                                         'White
                                         'King
                                         ('Info Z ('At 'E Nat1) 'False))
                                    ':-> ('Nothing
                                          ':-> ('Nothing
                                                ':-> ('Just
                                                        ('MkPiece
                                                           'White
                                                           'Rook
                                                           ('Info
                                                              Z
                                                              ('At
                                                                 'H
                                                                 (Nat1))
                                                              'False))
                                                      ':-> 'VEnd))))))))
           ':-> EmptyRow
                 ':-> EmptyRow
                       ':-> EmptyRow
                             ':-> EmptyRow
                                   ':-> EmptyRow
                                         ':-> EmptyRow
                                               ':-> (('Just
                                                        ('MkPiece
                                                           'Black
                                                           'Rook
                                                           ('Info
                                                              Z
                                                              ('At
                                                                 'A
                                                                 (Nat8))
                                                              'False))
                                                      ':-> ('Nothing
                                                            ':-> ('Nothing
                                                                  ':-> ('Nothing
                                                                        ':-> ('Just
                                                                                ('MkPiece
                                                                                   'Black
                                                                                   'King
                                                                                   ('Info
                                                                                      Z
                                                                                      ('At
                                                                                         'E
                                                                                         Nat8)
                                                                                      'False))
                                                                              ':-> ('Nothing
                                                                                    ':-> ('Nothing
                                                                                          ':-> ('Just
                                                                                                  ('MkPiece
                                                                                                     'Black
                                                                                                     'Rook
                                                                                                     ('Info
                                                                                                        Z
                                                                                                        ('At
                                                                                                           'H
                                                                                                           (Nat8))
                                                                                                        'False))
                                                                                                ':-> 'VEnd))))))))
                                                     ':-> 'VEnd))
          'Black
          ('At 'A Nat1)
          '( 'At 'E Nat1, 'At 'E Nat8)