module CastleHelperTests where

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
anySpaceInCheckTest1 :: False :~: Eval (AnySpaceInCheck White JustKingsDec (SpacesBetweenInc (At A Nat5) (At H Nat5)))
anySpaceInCheckTest1 = Refl

anySpaceInCheckTest2 :: Proxy (b :: BoardDecorator) -> Proxy (Eval (AnySpaceInCheck (GetMovingTeam b) b '[ At C Nat5 ]))
anySpaceInCheckTest2 (Proxy :: (Proxy (b :: BoardDecorator))) = Proxy @(Eval (AnySpaceInCheck (GetMovingTeam b) b '[ At C Nat5 ]))
aSICT2Board = create put _Wh _P at _c4 lastTeam _Wh end

anySpaceInCheckTest3 :: Proxy (b :: BoardDecorator) -> Proxy (Eval (AnySpaceInCheck (GetMovingTeam b) b '[ At B Nat5, At D Nat5 ]))
anySpaceInCheckTest3 (Proxy :: (Proxy (b :: BoardDecorator))) = Proxy @(Eval (AnySpaceInCheck (GetMovingTeam b) b '[ At B Nat5, At D Nat5 ]))

allSpacesFreeTest1 :: True :~: Eval (AllSpacesFreeOrKing White JustKingsDec (SpacesBetweenInc (At A Nat5) (At H Nat5)))
allSpacesFreeTest1 = Refl

allSpacesFreeTest2 :: Proxy (b :: BoardDecorator) -> Proxy (Eval (AllSpacesFreeOrKing White b '[ At C Nat4, At D Nat4 ]))
allSpacesFreeTest2 (Proxy :: (Proxy (b :: BoardDecorator)))
    = Proxy @(Eval (AllSpacesFreeOrKing b '[ At C Nat4, At D Nat4 ]))

allSpacesFreeTest3 :: True :~: Eval (AllSpacesFreeOrKing White JustKingsDec '[ At E Nat1 ])
allSpacesFreeTest3 = Refl

allSpacesFreeTest4 :: False :~: Eval (AllSpacesFreeOrKing Black JustKingsDec '[ At E Nat1 ])
allSpacesFreeTest4 = Refl

allSpacesFreeTest5 :: True :~: Eval (AllSpacesFreeOrKing Black JustKingsDec '[ At E Nat8 ])
allSpacesFreeTest5 = Refl

allSpacesFreeTest6 :: False :~: Eval (AllSpacesFreeOrKing White JustKingsDec '[ At E Nat8 ])
allSpacesFreeTest6 = Refl

allSpacesFreeTest7 :: '(False, False) :~: '( Eval (AllSpacesFreeOrKing White JustKingsDec (SpacesBetweenInc (At E Nat1) (At E Nat8))), Eval (AllSpacesFreeOrKing Black JustKingsDec (SpacesBetweenInc (At E Nat1) (At E Nat8))) )
allSpacesFreeTest7 = Refl

hasKingMovedTest1 :: False :~: HasKingMoved White JustKingsDec
hasKingMovedTest1 = Refl

hasKingMovedTest2 :: False :~: HasKingMoved Black JustKingsDec
hasKingMovedTest2 = Refl

moveKingsBoard = create
                     put _Wh _K at _e1
                     put _Bl _K at _e8
                     lastTeam _Bl
                  startMoves
                     king _e1 to _e2
                     king _e8 to _e7
                  end

hasKingMovedTest3 :: Proxy (b :: BoardDecorator) -> Proxy (HasKingMoved White b)
hasKingMovedTest3 (Proxy :: Proxy (b :: BoardDecorator))
    = Proxy @(HasKingMoved White b)

hasKingMovedTest4 :: Proxy (b :: BoardDecorator) -> Proxy (HasKingMoved Black b)
hasKingMovedTest4 (Proxy :: Proxy (b :: BoardDecorator))
    = Proxy @(HasKingMoved Black b)

haveRooksMovedTest1 :: '(False, False) :~: HaveRooksMoved White StartDec
haveRooksMovedTest1 = Refl

haveRooksMovedTest2 :: '(False, False) :~: HaveRooksMoved Black StartDec
haveRooksMovedTest2 = Refl

leftWRookRightBRookMovedBoard = create
                                    put _Wh _R at _a1
                                    put _Wh _R at _h1
                                    put _Bl _R at _a8
                                    put _Bl _R at _h8
                                    lastTeam _Bl
                                    lastMoved _e4
                                startMoves
                                    rook _a1 to _a2
                                    rook _h8 to _h7
                                end

bothWhiteRooksMovedBoard = create
                               put _Wh _R at _a1
                               put _Wh _R at _h1
                               put _Bl _P at _h7
                               lastTeam _Bl
                               lastMoved _e4  -- Set random position, to avoid nasty type errors
                           startMoves
                               rook _a1 to _a2
                               pawn _h7 to _h6
                               rook _h1 to _h2
                           end

rookBackToStartBoard = create
                           put _Bl _R at _a8
                           put _Bl _R at _h8
                           put _Wh _P at _a2
                           lastTeam _Wh
                           lastMoved _e4  -- Set random position to avoid nasty type errors
                        startMoves
                           rook _a8 to _a7
                           pawn _a2 to _a3
                           rook _a7 to _a8
                           pawn _a3 to _a4
                           rook _h8 to _h7
                        end


haveRooksMovedTest3 :: Proxy (b :: BoardDecorator) -> Proxy (Eval ('(True, False) :==: (HaveRooksMoved White b)))
haveRooksMovedTest3 (Proxy :: Proxy (b :: BoardDecorator))
    = Proxy @(Eval ('(True, False) :==: (HaveRooksMoved White b)))

haveRooksMovedTest4 :: Proxy (b :: BoardDecorator) -> Proxy (Eval ('(False, True) :==: (HaveRooksMoved Black b)))
haveRooksMovedTest4 (Proxy :: Proxy (b :: BoardDecorator))
    = Proxy @(Eval ('(False, True) :==: (HaveRooksMoved Black b)))

haveRooksMovedTest5 :: Proxy (b :: BoardDecorator) -> Proxy (Eval ('(True, True) :==: (HaveRooksMoved White b)))
haveRooksMovedTest5 (Proxy :: Proxy (b :: BoardDecorator))
    = Proxy @(Eval ('(True, True) :==: (HaveRooksMoved White b)))
   
haveRooksMovedTest6 :: Proxy (b :: BoardDecorator) -> Proxy (Eval ('(True, True) :==: (HaveRooksMoved Black b)))
haveRooksMovedTest6 (Proxy :: Proxy (b :: BoardDecorator))
    = Proxy @(Eval ('(True, True) :==: (HaveRooksMoved Black b)))

castleHelperTestSuite = describe "Castle Helper Function Tests" $ do
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
            it "3: A position with a White King in should be registered as free for White" $
               shouldTypecheck allSpacesFreeTest3
            it "4: A position with a White King in should NOT be registered as free for Black" $
               shouldTypecheck allSpacesFreeTest4
            it "5: A position with a Black King in should be registered as free for Black" $
               shouldTypecheck allSpacesFreeTest5
            it "6: A position with a Black King in should NOT be registered as free for White" $
               shouldTypecheck allSpacesFreeTest6
            it "7: A list of positions, with both White King and Black King in, should not be registered as free for either" $
               shouldTypecheck allSpacesFreeTest7
        describe "HasKingMoved Tests" $ do
            it "1: If the White King has not moved, HasKingMoved White should return False" $
               shouldTypecheck hasKingMovedTest1
            it "2: If the Black King has not moved, HasKingMoved Black should return False" $
               shouldTypeCheck hasKingMovedTest2
            it "3: If the White King has moved, HasKingMoved White should return True" $
               shouldTypecheck $ fromProxyTrue (hasKingMovedTest3 moveKingsBoard)
            it "4: If the Black King has moved, HasKingMoved Black should return True" $
               shouldTypeCheck $ fromProxyTrue (hasKingMovedTest4 moveKingsBoard)
        describe "HaveRooksMoved Tests" $ do
            it "1: If neither White Rook has moved, HaveRooksMoved White should return '(False, False)" $
               shouldTypeCheck haveRooksMovedTest1
            it "2: If neither Black Rook has moved, HaveRooksMoved Black should return '(False, False)" $
               shouldTypeCheck haveRooksMovedTest2
            it "3: If the left Rook has moved, but the right one hasn't, HaveRooksMoved should return '(True, False)" $
               shouldTypeCheck $ fromProxyTrue (haveRooksMovedTest3 leftWRookRightBRookMovedBoard)
            it "4: If the right Rook has moved, but the left one hasn't, HaveRooksMoved should return '(False, True)" $
               shouldTypeCheck $ fromProxyTrue (haveRooksMovedTest4 leftWRookRightBRookMovedBoard)
            it "5: If both Rooks have moved, HaveRooksMoved should return '(True, True)" $
               shouldTypeCheck $ fromProxyTrue (haveRooksMovedTest5 bothWhiteRooksMovedBoard)
            it "6: If the left rook moves back to its start position, and if the right rook is not present, then HaveRooksMoved should return '(True, True)" $
               shouldTypeCheck $ fromProxyTrue (haveRooksMovedTest6 rookBackToStartBoard)

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