module CanCastleTests where

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
import CastleHelperTests

canCastleTestRefl1 :: '( '(False, False), '(False, False) ) :~: '(CanCastle White StartDec, CanCastle Black StartDec)
canCastleTestRefl1 = Refl

canCastleTestRefl2 :: '(True, True) :~: CanCastle White WhiteCanCastleDec
canCastleTestRefl2 = Refl

canCastleTestRefl3 :: '( False, False) :~: CanCastle Black WhiteCanCastleDec
canCastleTestRefl3 = Refl

canCastleTestRefl4 :: '(True, False) :~: CanCastle Black BlackLeftCastleWhiteRightCastleDec
canCastleTestRefl4 = Refl

canCastleTestRefl5 :: '(False, True) :~: CanCastle White BlackLeftCastleWhiteRightCastleDec
canCastleTestRefl5 = Refl

canCastleTest1 :: Test.Hspec.Spec
canCastleTest1 = it "1: In the starting board configuration for chess, neither Black nor White can castle." $
   shouldTypecheck canCastleTestRefl1

canCastleTest2 :: Test.Hspec.Spec
canCastleTest2 = it "2: If there are no pieces between the White King and its' Rooks, and none of them have moved, White can castle." $
   shouldTypecheck canCastleTestRefl2

canCastleTest3 :: Test.Hspec.Spec
canCastleTest3 = it "3: If there are pieces between the Black King and its' Rooks, Black cannot castle." $
   shouldTypecheck canCastleTestRefl3

canCastleTest4 :: Test.Hspec.Spec
canCastleTest4 = it "4: Even if Black can only castle to the left, it should still be able to castle to the right." $
   shouldTypecheck canCastleTestRefl4

canCastleTest5 :: Test.Hspec.Spec
canCastleTest5 = it "5: Even if White can only castle to the right, it should still be able to castle to the left." $
   shouldTypecheck canCastleTestRefl5

canCastleTestSuite :: Test.Hspec.Spec
canCastleTestSuite = describe "CanCastle Tests" $ do
   canCastleTest1
   canCastleTest2
   canCastleTest3
   canCastleTest4
   canCastleTest5

-------------------------------------------------------------------------------------------------------
-- MESSY DECLARATIONS
-------------------------------------------------------------------------------------------------------

type WhiteCanCastleDec = 'Dec
          ((('Just
              ('MkPiece
                 'White
                 'Rook
                 ('Info
                    Z ('At 'A Nat1) ))) :-> ('Nothing :-> ('Nothing :-> ('Nothing :-> ('Just
                                      ('MkPiece
                                         'White
                                         'King
                                         ('Info Z ('At 'E Nat1) ))) :-> ('Nothing :-> ('Nothing :<> ('Just
                                                        ('MkPiece
                                                           'White
                                                           'Rook
                                                           ('Info
                                                              Z
                                                              ('At
                                                                 'H
                                                                 Nat1)
                                                              )))))))))
           :-> EmptyRow
                 :-> EmptyRow
                       :-> EmptyRow
                              :-> EmptyRow
                                   :-> EmptyRow
                                         :-> EmptyRow
                                               :<> (('Just
                                                        ('MkPiece
                                                           'Black
                                                           'Rook
                                                           ('Info
                                                              Z
                                                              ('At
                                                                 'A
                                                                 Nat8)
                                                              )))
                                                      :-> ('Just
                                                              ('MkPiece
                                                                 'Black
                                                                 'Pawn
                                                                 ('Info
                                                                    Z
                                                                    ('At
                                                                       'B Nat8)
                                                                    )))
                                                            :-> ('Nothing
                                                                  :-> ('Nothing
                                                                        :-> ('Just
                                                                                ('MkPiece
                                                                                   'Black
                                                                                   'King
                                                                                   ('Info
                                                                                      Z
                                                                                      ('At
                                                                                         'E
                                                                                         Nat8)
                                                                                      )))
                                                                              :-> ('Nothing
                                                                                    :-> ('Just
                                                                                            ('MkPiece
                                                                                               'Black
                                                                                               'Pawn
                                                                                               ('Info
                                                                                                  Z
                                                                                                  ('At
                                                                                                     'G
                                                                                                     Nat8)
                                                                                                  )))
                                                                                          :<> ('Just
                                                                                                  ('MkPiece
                                                                                                     'Black
                                                                                                     'Rook
                                                                                                     ('Info
                                                                                                        Z
                                                                                                        ('At
                                                                                                           'H
                                                                                                           Nat8)
                                                                                                        ))))))))
          'Black
          ('At 'E Nat4)
          '( 'At 'E Nat1, 'At 'E Nat8) Nat1

type BlackLeftCastleWhiteRightCastleDec = 'Dec
          ((('Just
              ('MkPiece
                 'White
                 'Rook
                 ('Info
                    Z ('At 'A Nat1) ))) :-> ((Just (MkPiece White Pawn (Info Z (At A Nat2)))) :-> ('Nothing :-> ('Nothing :-> ('Just
                                      ('MkPiece
                                         'White
                                         'King
                                         ('Info Z ('At 'E Nat1) ))) :-> ('Nothing :-> ('Nothing :<> ('Just
                                                        ('MkPiece
                                                           'White
                                                           'Rook
                                                           ('Info
                                                              Z
                                                              ('At
                                                                 'H
                                                                 Nat1)
                                                              )))))))))
           :-> EmptyRow
                 :-> EmptyRow
                       :-> EmptyRow
                              :-> EmptyRow
                                   :-> EmptyRow
                                         :-> EmptyRow
                                               :<> (('Just
                                                        ('MkPiece
                                                           'Black
                                                           'Rook
                                                           ('Info
                                                              Z
                                                              ('At
                                                                 'A
                                                                 Nat8)
                                                              )))
                                                      :-> Nothing
                                                            :-> ('Nothing
                                                                  :-> ('Nothing
                                                                        :-> ('Just
                                                                                ('MkPiece
                                                                                   'Black
                                                                                   'King
                                                                                   ('Info
                                                                                      Z
                                                                                      ('At
                                                                                         'E
                                                                                         Nat8)
                                                                                      )))
                                                                              :-> ('Nothing
                                                                                    :-> ('Just
                                                                                            ('MkPiece
                                                                                               'Black
                                                                                               'Pawn
                                                                                               ('Info
                                                                                                  Z
                                                                                                  ('At
                                                                                                     'G
                                                                                                     Nat8)
                                                                                                  )))
                                                                                          :<> ('Just
                                                                                                  ('MkPiece
                                                                                                     'Black
                                                                                                     'Rook
                                                                                                     ('Info
                                                                                                        Z
                                                                                                        ('At
                                                                                                           'H
                                                                                                           Nat8)
                                                                                                        ))))))))
          'Black
          ('At 'E Nat4)
          '( 'At 'E Nat1, 'At 'E Nat8) Nat1