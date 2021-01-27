module FamousGames where

import FlatBuilders
import MakeSingletons
import qualified GHC.TypeLits as TL
import ChessTypes
import FirstClassFunctions
import Lib

-- kholmovVsBronstein = chess
--     pawn _e2 to _e4
--     pawn _c7 to _c5
--     knight _g1 to _f3
--     knight _g8 to _f6
--     knight _b1 to _c3
--     pawn _d7 to _d6 -- end  -- Successfully compiles, with <= 22GB memory usage.
--     pawn _d2 to _d4 -- end
--     pawn _c5 to _d4 end  -- FIXME: Too much memory usage.
--     knight _f3 to _d4
--     pawn _a7 to _a6 end  -- Finishes compiling in about 3 minutes!
    -- bishop _c1 to _g5 end
    -- pawn _e7 to _e6
    -- pawn _f2 to _f4
    -- bishop _f8 to _e7
    -- queen _d1 to _f3
    -- queen _d8 to _c7
    -- king _e1 to _c1
    -- knight _b8 to _d7
    -- pawn _g2 to _g4  -- Move 10 - absolutely tanks compiler
    -- pawn _b7 to _b5
-- end
