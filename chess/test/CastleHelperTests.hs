module CastleHelperTests where

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

-- data AnySpaceInCheck :: BoardDecorator -> [Position] -> Exp Bool
anySpaceInCheckTest1 :: False :~: Eval (AnySpaceInCheck White JustKingsDec (SpacesBetweenInc (At A Nat5) (At H Nat5)))
anySpaceInCheckTest1 = Refl

anySpaceInCheckTest2 :: Proxy (b :: BoardDecorator) -> Proxy (Eval (AnySpaceInCheck (GetMovingTeam b) b '[ At C Nat5 ]))
anySpaceInCheckTest2 (Proxy :: (Proxy (b :: BoardDecorator))) = Proxy @(Eval (AnySpaceInCheck (GetMovingTeam b) b '[ At C Nat5 ]))
aSICT2Board = create put _Wh _P at c4 lastTeam _Wh end

anySpaceInCheckTest3 :: Proxy (b :: BoardDecorator) -> Proxy (Eval (AnySpaceInCheck (GetMovingTeam b) b '[ At B Nat5, At D Nat5 ]))
anySpaceInCheckTest3 (Proxy :: (Proxy (b :: BoardDecorator))) = Proxy @(Eval (AnySpaceInCheck (GetMovingTeam b) b '[ At B Nat5, At D Nat5 ]))

allSpacesFreeTest1 :: True :~: Eval (AllSpacesFree JustKingsDec (SpacesBetweenInc (At A Nat5) (At H Nat5)))
allSpacesFreeTest1 = Refl

allSpacesFreeTest2 :: Proxy (b :: BoardDecorator) -> Proxy (Eval (AllSpacesFree b '[ At C Nat4, At D Nat4 ]))
allSpacesFreeTest2 (Proxy :: (Proxy (b :: BoardDecorator)))
    = Proxy @(Eval (AllSpacesFree b '[ At C Nat4, At D Nat4 ]))

allSpacesFreeTest3 :: '(False, False) :~: '( Eval (AllSpacesFree JustKingsDec (SpacesBetweenInc (At E Nat1) (At E Nat8))), Eval (AllSpacesFree JustKingsDec (SpacesBetweenInc (At E Nat1) (At E Nat8))) )
allSpacesFreeTest3 = Refl

hasKingMovedTest1 :: False :~: HasKingMoved White JustKingsDec
hasKingMovedTest1 = Refl

hasKingMovedTest2 :: False :~: HasKingMoved Black JustKingsDec
hasKingMovedTest2 = Refl

hasKingMovedTest3 :: True :~: HasKingMoved White MoveKingsDec
hasKingMovedTest3 = Refl

hasKingMovedTest4 :: True :~: HasKingMoved Black MoveKingsDec
hasKingMovedTest4 = Refl

haveRooksMovedTest1 :: '(False, False) :~: HaveRooksMoved White StartDec
haveRooksMovedTest1 = Refl

haveRooksMovedTest2 :: '(False, False) :~: HaveRooksMoved Black StartDec
haveRooksMovedTest2 = Refl

-- bothWhiteRooksMovedBoard = create
--                                put _Wh _R at a1
--                                put _Wh _R at h1
--                                put _Bl _P at h7
--                                lastTeam _Bl
--                                lastMoved e4  -- Set random position, to avoid nasty type errors
--                            startMoves
--                                rook a1 to a2
--                                pawn h7 to h6
--                                rook h1 to h2
--                            end

-- rookBackToStartBoard = create
--                            put _Bl _R at a8
--                            put _Bl _R at h8
--                            put _Wh _P at a2
--                            lastTeam _Wh
--                            lastMoved e4  -- Set random position to avoid nasty type errors
--                         startMoves
--                            rook a8 to a7
--                            pawn a2 to a3
--                            rook a7 to a8
--                            pawn a3 to a4
--                            rook h8 to h7
--                         end


haveRooksMovedTest3 :: '(True, False) :~: HaveRooksMoved White LeftWRookRightBRookDec
haveRooksMovedTest3 = Refl

haveRooksMovedTest4 :: '(False, True) :~: HaveRooksMoved Black LeftWRookRightBRookDec
haveRooksMovedTest4 = Refl

haveRooksMovedTest5 :: Proxy (b :: BoardDecorator) -> Proxy (Eval ('(True, True) :==: (HaveRooksMoved White b)))
haveRooksMovedTest5 (Proxy :: Proxy (b :: BoardDecorator))
    = Proxy @(Eval ('(True, True) :==: (HaveRooksMoved White b)))
   
haveRooksMovedTest6 :: Proxy (b :: BoardDecorator) -> Proxy (Eval ('(True, True) :==: (HaveRooksMoved Black b)))
haveRooksMovedTest6 (Proxy :: Proxy (b :: BoardDecorator))
    = Proxy @(Eval ('(True, True) :==: (HaveRooksMoved Black b)))

a1 :: Test.Hspec.Spec
a1 = it "1" $
      shouldTypeCheck anySpaceInCheckTest1

a2 :: Test.Hspec.Spec
a2 = it "2: A space above a White Pawn should NOT be in check" $
      shouldTypeCheck $ fromProxyFalse (anySpaceInCheckTest2 aSICT2Board)

a3 :: Test.Hspec.Spec
a3 = it "3: The spaces diagonally above a White Pawn should be in check" $
      shouldTypeCheck $ fromProxyTrue (anySpaceInCheckTest3 aSICT2Board) 

alpha :: Test.Hspec.Spec
alpha = describe "AnySpaceInCheck Tests" $ do
      a1 
      a2 
      a3 

b1 :: Test.Hspec.Spec
b1 = it "1: An empty row should be registered as free" $
      shouldTypeCheck allSpacesFreeTest1

b2 :: Test.Hspec.Spec
b2 = it "2: A position with a Pawn in should NOT be registered as free" $
      shouldTypeCheck $ fromProxyFalse (allSpacesFreeTest2 aSICT2Board)

b3 :: Test.Hspec.Spec 
b3 = it "3: A list of positions, with both White King and Black King in, should not be registered as free" $
      shouldTypecheck allSpacesFreeTest3

beta :: Test.Hspec.Spec
beta = describe "AllSpacesFree Tests" $ do
      b1 
      b2 
      b3 

g1 :: Test.Hspec.Spec
g1 = it "1: If the White King has not moved, HasKingMoved White should return False" $
      shouldTypecheck hasKingMovedTest1

g2 :: Test.Hspec.Spec
g2 = it "2: If the Black King has not moved, HasKingMoved Black should return False" $
      shouldTypeCheck hasKingMovedTest2

g3 :: Test.Hspec.Spec
g3 = it "3: If the White King has moved, HasKingMoved White should return True" $
      shouldTypecheck hasKingMovedTest3

g4 :: Test.Hspec.Spec
g4 = it "4: If the Black King has moved, HasKingMoved Black should return True" $
      shouldTypeCheck hasKingMovedTest4

gamma :: Test.Hspec.Spec
gamma = describe "HasKingMoved Tests" $ do
      g1 
      g2 
      g3 
      g4 

d1 :: Test.Hspec.Spec
d1 = it "1: If neither White Rook has moved, HaveRooksMoved White should return '(False, False)" $
      shouldTypeCheck haveRooksMovedTest1

d2 :: Test.Hspec.Spec
d2 = it "2: If neither Black Rook has moved, HaveRooksMoved Black should return '(False, False)" $
      shouldTypeCheck haveRooksMovedTest2

d3 :: Test.Hspec.Spec
d3 = it "3: If the left Rook has moved, but the right one hasn't, HaveRooksMoved should return '(True, False)" $
      shouldTypeCheck haveRooksMovedTest3

d4 :: Test.Hspec.Spec
d4 = it "4: If the right Rook has moved, but the left one hasn't, HaveRooksMoved should return '(False, True)" $
      shouldTypeCheck haveRooksMovedTest4

-- -- TODO: Find a way to include??
-- d5 :: Test.Hspec.Spec
-- d5 = it "5: If both Rooks have moved, HaveRooksMoved should return '(True, True)" $
--       shouldTypeCheck $ fromProxyTrue (haveRooksMovedTest5 bothWhiteRooksMovedBoard)

-- d6 :: Test.Hspec.Spec
-- d6 = it "6: If the left rook moves back to its start position, and if the right rook is not present, then HaveRooksMoved should return '(True, True)" $
--       shouldTypeCheck $ fromProxyTrue (haveRooksMovedTest6 rookBackToStartBoard)

delta :: Test.Hspec.Spec
delta = describe "HaveRooksMoved Tests" $ do
      d1 
      d2
      d3
      d4
      -- d5
      -- d6    
      
castleHelperTestSuite :: Test.Hspec.Spec
castleHelperTestSuite = describe "Castle Helper Function Tests" $ do
      alpha 
      beta
      gamma
      delta

-------------------------------------------------------------------------------------------------------
-- MESSY DECLARATIONS
-------------------------------------------------------------------------------------------------------

-- onlyRooksAndKings = create
--                         put _Wh _R at a1
--                         put _Wh _R at h1
--                         put _Bl _R at a8
--                         put _Bl _R at h8
--                     end

-- Used above EDSL statement to get it in repl, then copied and pasted!
type OnlyRooksAndKings = 'Dec
          (('Just
              ('MkPiece
                 'White
                 'Rook
                 ('Info
                    Z ('At 'A (Nat1))))
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
                                                                 (Nat1))
                                                            ))
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
                                                                                                           (Nat8))
                                                                                                      ))
                                                                                                ':-> 'VEnd))))))))
                                                     ':-> 'VEnd))
          'Black
          ('At 'A Nat1)
          '( 'At 'E Nat1, 'At 'E Nat8) Nat1

-- moveKingsBoard = create
--                      put _Wh _K at e1
--                      put _Bl _K at e8
--                      lastTeam _Bl
--                   startMoves
--                      king e1 to e2
--                      king e8 to e7
--                   end

type MoveKingsDec = 'Dec
          (('Nothing
            ':-> ('Nothing
                  ':-> ('Nothing
                        ':-> ('Nothing
                              ':-> ('Nothing
                                    ':-> ('Nothing ':-> ('Nothing ':-> ('Nothing ':-> 'VEnd))))))))
           ':-> (('Nothing
                  ':-> ('Nothing
                        ':-> ('Nothing
                              ':-> ('Nothing
                                    ':-> ('Just
                                            ('MkPiece
                                               'White
                                               'King
                                               ('Info
                                                  (S Z)
                                                  ('At 'E (S Nat1))
                                                ))
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
                                                                  ':-> ('Just
                                                                          ('MkPiece
                                                                             'Black
                                                                             'King
                                                                             ('Info
                                                                                (S
                                                                                   Z)
                                                                                ('At
                                                                                   'E
                                                                                   (S
                                                                                      Nat6))
                                                                              ))
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
                                                     ':-> 'VEnd))))))))
          'Black
          ('At 'E Nat7)
          '( 'At 'E Nat2, 'At 'E Nat7) Nat1

-- leftWRookRightBRookMovedBoard = create
--                                     put _Wh _R at a1
--                                     put _Wh _R at h1
--                                     put _Bl _R at a8
--                                     put _Bl _R at h8
--                                     lastTeam _Bl
--                                     lastMoved e4
--                                 startMoves
--                                     rook a1 to a2
--                                     rook h8 to h7
--                                 end

type LeftWRookRightBRookDec = 'Dec
          (('Nothing
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
           ':-> (('Just
                    ('MkPiece
                       'White
                       'Rook
                       ('Info
                          (S Z)
                          ('At 'A (S Nat1))
                        ))
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
                                                                                    ':-> ('Just
                                                                                            ('MkPiece
                                                                                               'Black
                                                                                               'Rook
                                                                                               ('Info
                                                                                                  (S
                                                                                                     Z)
                                                                                                  ('At
                                                                                                     'H
                                                                                                     (S
                                                                                                        Nat6))
                                                                                                ))
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
                                                                                          ':-> ('Nothing
                                                                                                ':-> 'VEnd))))))))
                                                     ':-> 'VEnd))))))))
          'Black
          ('At 'H Nat7)
          '( 'At 'E Nat1, 'At 'E Nat8) Nat1