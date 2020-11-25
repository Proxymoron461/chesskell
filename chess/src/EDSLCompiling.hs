module EDSLCompiling where

import FlatBuilders
import MakeSingletons
import qualified GHC.TypeLits as TL
import ChessTypes
import FirstClassFunctions

-- SUCCESS
-- wrongPieceError = chess rook _a2 to _a3 end

-- SUCCESS
-- invalidMoveErrorCantMoveTo = chess pawn _a2 to _a6 end

-- SUCCESS
-- invalidMoveErrorEmpty = chess rook _b4 to _b5 end

-- -- SUCCESS - EDSL stops and reports at first error
-- carryError = chess pawn _a2 to _a6 pawn _h7 to _h6 end

-- -- SUCCESS - test failed
-- errorAfterMoves = chess pawn _a2 to _a3 pawn _a7 to _a6 pawn _a3 to _a5 end

-- type TestType = TL.TypeError (TL.Text "Bleh")

-- -- SUCCESS - type error
-- didntPromoteBlack = create
--         put _Wh _P at _h7
--         put _Bl _P at _a2
--     startMoves
--         pawn _h7 promoteTo _B _h8
--         pawn _a2 to _a1
--     end

-- -- SUCCESS - type error
-- didntPromoteWhite = create
--         put _Wh _P at _h7
--         put _Bl _P at _a2
--     startMoves
--         pawn _h7 to _h8
--     end