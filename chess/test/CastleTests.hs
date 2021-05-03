module CastleTests where

import Test.Hspec
import Test.HUnit.Lang (Assertion, assertFailure)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Data.Type.Equality ((:~:)(..))
import qualified GHC.TypeLits as TL (Nat)
import Data.Proxy(Proxy(..))
import Data.Type.Nat hiding (SNat(..))

import Chesskell
import Vec
import FirstClassFunctions
import ChessTypes
import FlatBuilders
import MakeProxies

import TestTypes
import CanCastleTests
import CastleHelperTests

type WhiteCastleLeftDec  = Eval (Move (At E Nat1) (At C Nat1) CastleDec)
type WhiteCastleRightDec = Eval (Move (At E Nat1) (At G Nat1) CastleDec)

-- castleTest1 :: '(True, True) :~: '(Eval (IsKingAt White WhiteCastleLeftDec (At C Nat1)), Eval (IsRookAt White WhiteCastleLeftDec (At D Nat1)))
-- castleTest1 = Refl

-- castleTest2 :: '(True, True) :~: '(Eval (IsKingAt White WhiteCastleLeftDec (At G Nat1)), Eval (IsRookAt White WhiteCastleLeftDec (At F Nat1)))
-- castleTest2 = Refl

type BlackCastleLeftDec  = Eval (Move (At E Nat8) (At C Nat8) (SetLastTeam CastleDec White))
type BlackCastleRightDec = Eval (Move (At E Nat8) (At G Nat8) (SetLastTeam CastleDec White))

-- castleTest3 :: '(True, True) :~: '(Eval (IsKingAt Black BlackCastleLeftDec (At C Nat8)), Eval (IsRookAt Black BlackCastleLeftDec (At D Nat8)))
-- castleTest3 = Refl

-- castleTest4 :: '(True, True) :~: '(Eval (IsKingAt Black BlackCastleLeftDec (At G Nat8)), Eval (IsRookAt Black BlackCastleLeftDec (At F Nat8)))
-- castleTest4 = Refl

castleTestSuite = describe "Castle Tests" $ do
    castleHelperTestSuite
    canCastleTestSuite
--     describe "Castling tests" $ do
--         it "1: If White castles to the left, the White King should be at C1, with the Rook at D1." $
--             shouldTypeCheck castleTest1
--         it "2: If White castles to the right, the White King should be at G1, with the Rook at F1." $
--             shouldTypeCheck castleTest2
--         it "3: If Black castles to the left, the Black King should be at C8, with the Rook at D8." $
--             shouldTypeCheck castleTest3
--         it "4: If Black castles to the right, the Black King should be at G8, with the Rook at F8." $
--             shouldTypeCheck castleTest4

-------------------------------------------------------------------------------------------------------
-- MESSY DECLARATIONS
-------------------------------------------------------------------------------------------------------

type CastleDec = 'Dec
          (('Just
              ('MkPiece
                 'White
                 'Rook
                 ('Info
                    Z ('At 'A (S Nat0))))
            ':-> ('Nothing
                  ':-> ('Nothing
                        ':-> ('Nothing
                              ':-> ('Just
                                      ('MkPiece
                                         'White
                                         'King
                                         ('Info Z ('At 'E Nat1)))
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
                                                                 (S Nat0))
                                                            ))
                                                      ':-> 'VEnd))))))))
           ':-> (('Nothing
                  ':-> ('Nothing
                        ':-> ('Nothing
                              ':-> ('Nothing
                                    ':-> ('Nothing
                                          ':-> ('Nothing
                                                ':-> ('Nothing ':-> ('Nothing ':-> 'VEnd))))))))
                 ':-> (('Nothing
                        ':-> ('Nothing
                              ':-> ('Nothing
                                    ':-> ('Nothing
                                          ':-> ('Nothing
                                                ':-> ('Nothing
                                                      ':-> ('Nothing
                                                            ':-> ('Nothing ':-> 'VEnd))))))))
                       ':-> (('Nothing
                              ':-> ('Nothing
                                    ':-> ('Nothing
                                          ':-> ('Nothing
                                                ':-> ('Nothing
                                                      ':-> ('Nothing
                                                            ':-> ('Nothing
                                                                  ':-> ('Nothing ':-> 'VEnd))))))))
                             ':-> (('Nothing
                                    ':-> ('Nothing
                                          ':-> ('Nothing
                                                ':-> ('Nothing
                                                      ':-> ('Nothing
                                                            ':-> ('Nothing
                                                                  ':-> ('Nothing
                                                                        ':-> ('Nothing
                                                                              ':-> 'VEnd))))))))
                                   ':-> (('Nothing
                                          ':-> ('Nothing
                                                ':-> ('Nothing
                                                      ':-> ('Nothing
                                                            ':-> ('Nothing
                                                                  ':-> ('Nothing
                                                                        ':-> ('Nothing
                                                                              ':-> ('Nothing
                                                                                    ':-> 'VEnd))))))))
                                         ':-> (('Nothing
                                                ':-> ('Nothing
                                                      ':-> ('Nothing
                                                            ':-> ('Nothing
                                                                  ':-> ('Nothing
                                                                        ':-> ('Nothing
                                                                              ':-> ('Nothing
                                                                                    ':-> ('Nothing
                                                                                          ':-> 'VEnd))))))))
                                               ':-> (('Just
                                                        ('MkPiece
                                                           'Black
                                                           'Rook
                                                           ('Info
                                                              Z
                                                              ('At
                                                                 'A
                                                                 (S Nat7))
                                                            ))
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
                                                                                    ))
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
                                                                                                           (S
                                                                                                              Nat7))
                                                                                                      ))
                                                                                                ':-> 'VEnd))))))))
                                                     ':-> 'VEnd))))))))
          'White
          ('At 'A Nat1)
          '( 'At 'E Nat1, 'At 'E Nat8) Nat1