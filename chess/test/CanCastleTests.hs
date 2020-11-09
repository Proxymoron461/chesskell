module CanCastleTests where

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
import CastleHelperTests

canCastleTest1 :: '( '(False, False), '(False, False) ) :~: '(CanCastle White StartDec, CanCastle Black StartDec)
canCastleTest1 = Refl

canCastleTest2 :: '(True, True) :~: CanCastle White WhiteCanCastleDec
canCastleTest2 = Refl

canCastleTest3 :: '( False, False) :~: CanCastle Black WhiteCanCastleDec
canCastleTest3 = Refl

canCastleTest4 :: '(True, False) :~: CanCastle Black BlackLeftCastleWhiteRightCastleDec
canCastleTest4 = Refl

canCastleTest5 :: '(False, True) :~: CanCastle White BlackLeftCastleWhiteRightCastleDec
canCastleTest5 = Refl

canCastleTestSuite = describe "CanCastle Tests" $ do
        it "1: In the starting board configuration for chess, neither Black nor White can castle." $
            shouldTypecheck canCastleTest1
        it "2: If there are no pieces between the White King and its' Rooks, and none of them have moved, White can castle." $
            shouldTypecheck canCastleTest2
        it "3: If there are pieces between the Black King and its' Rooks, Black cannot castle." $
            shouldTypecheck canCastleTest3
        it "4: Even if Black can only castle to the left, CanCastle Black should return true." $
            shouldTypecheck canCastleTest4
        it "5: Even if White can only castle to the right, CanCastle White should return true." $
            shouldTypecheck canCastleTest5
   --  describe "Castling tests" $ do
   --      it blah blah blah

-------------------------------------------------------------------------------------------------------
-- MESSY DECLARATIONS
-------------------------------------------------------------------------------------------------------

type WhiteCanCastleDec = 'Dec
          ((('Just
              ('MkPiece
                 'White
                 'Rook
                 ('Info
                    Z ('At 'A Nat1) 'False))) :-> ('Nothing :-> ('Nothing :-> ('Nothing :-> ('Just
                                      ('MkPiece
                                         'White
                                         'King
                                         ('Info Z ('At 'E Nat1) 'False))) :-> ('Nothing :-> ('Nothing :<> ('Just
                                                        ('MkPiece
                                                           'White
                                                           'Rook
                                                           ('Info
                                                              Z
                                                              ('At
                                                                 'H
                                                                 Nat1)
                                                              'False)))))))))
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
                                                              'False)))
                                                      :-> ('Just
                                                              ('MkPiece
                                                                 'Black
                                                                 'Pawn
                                                                 ('Info
                                                                    Z
                                                                    ('At
                                                                       'B Nat8)
                                                                    'False)))
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
                                                                                      'False)))
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
                                                                                                  'False)))
                                                                                          :<> ('Just
                                                                                                  ('MkPiece
                                                                                                     'Black
                                                                                                     'Rook
                                                                                                     ('Info
                                                                                                        Z
                                                                                                        ('At
                                                                                                           'H
                                                                                                           Nat8)
                                                                                                        'False))))))))
          'Black
          ('At 'E Nat4)
          '( 'At 'E Nat1, 'At 'E Nat8)

type BlackLeftCastleWhiteRightCastleDec = 'Dec
          ((('Just
              ('MkPiece
                 'White
                 'Rook
                 ('Info
                    Z ('At 'A Nat1) 'False))) :-> ((Just (MkPiece White Pawn (Info Z (At A Nat2) False))) :-> ('Nothing :-> ('Nothing :-> ('Just
                                      ('MkPiece
                                         'White
                                         'King
                                         ('Info Z ('At 'E Nat1) 'False))) :-> ('Nothing :-> ('Nothing :<> ('Just
                                                        ('MkPiece
                                                           'White
                                                           'Rook
                                                           ('Info
                                                              Z
                                                              ('At
                                                                 'H
                                                                 Nat1)
                                                              'False)))))))))
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
                                                              'False)))
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
                                                                                      'False)))
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
                                                                                                  'False)))
                                                                                          :<> ('Just
                                                                                                  ('MkPiece
                                                                                                     'Black
                                                                                                     'Rook
                                                                                                     ('Info
                                                                                                        Z
                                                                                                        ('At
                                                                                                           'H
                                                                                                           Nat8)
                                                                                                        'False))))))))
          'Black
          ('At 'E Nat4)
          '( 'At 'E Nat1, 'At 'E Nat8)