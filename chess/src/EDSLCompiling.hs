module EDSLCompiling where

import FlatBuilders
import MakeProxies
import qualified GHC.TypeLits as TL
import ChessTypes
import FirstClassFunctions

-- SUCCESS
-- wrongPieceError = chess rook a2 to a3 end

-- SUCCESS
-- invalidMoveErrorCantMoveTo = chess pawn a2 to a6 end

-- SUCCESS
-- invalidMoveErrorEmpty = chess rook b4 to b5 end

-- -- SUCCESS - EDSL stops and reports at first error
-- carryError = chess pawn a2 to a6 pawn h7 to h6 end

-- -- SUCCESS - test failed
-- errorAfterMoves = chess pawn a2 to a3 pawn a7 to a6 pawn a3 to a5 end

-- type TestType = TL.TypeError (TL.Text "Bleh")

-- -- SUCCESS - type error
-- didntPromoteBlack = create
--         put _Wh _P at h7
--         put _Bl _P at a2
--     startMoves
--         pawn h7 promoteTo _B h8
--         pawn a2 to a1
--     end

-- -- SUCCESS - type error
-- didntPromoteWhite = create
--         put _Wh _P at h7
--         put _Bl _P at a2
--     startMoves
--         pawn h7 to h8
--     end

-- FINE
-- castle = create put _Wh _R at a1 put _Wh _R at h1 put _Bl _R at a8 put _Bl _R at h8 startMoves o_o o_o_o end
-- castle = create put _Wh _R at a1 put _Wh _R at h1 put _Bl _R at a8 put _Bl _R at h8 startMoves o_o_o o_o end
-- castle = create put _Wh _R at a1 put _Wh _R at h1 put _Bl _R at a8 put _Bl _R at h8 startMoves o_o_o o_o_o end
-- castle = create put _Wh _R at a1 put _Wh _R at h1 put _Bl _R at a8 put _Bl _R at h8 startMoves o_o o_o end