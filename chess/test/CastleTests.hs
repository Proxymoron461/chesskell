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
import CastleHelperTests

canCastleTest1 :: '(False, False) :~: '(CanCastle White StartDec, CanCastle Black StartDec)
canCastleTest1 = Refl

canCastleTest2 :: True :~: CanCastle White WhiteCanCastleDec
canCastleTest2 = Refl

canCastleTest3 :: False :~: CanCastle Black WhiteCanCastleDec
canCastleTest3 = Refl

castleTestSuite = describe "Castle Tests" $ do
    castleHelperTestSuite
    describe "CanCastle Tests" $ do
        it "1: In the starting board configuration for chess, neither Black nor White can castle." $
            shouldTypecheck canCastleTest1
        it "2: If there are no pieces between the White King and its' Rooks, and none of them have moved, White can castle." $
            shouldTypecheck canCastleTest2
        it "3: If there are pieces between the Black King and its' Rooks, Black cannot castle." $
            shouldTypecheck canCastleTest3
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