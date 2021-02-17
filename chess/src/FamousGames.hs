module FamousGames where

import FlatBuilders
import MakeSingletons
import qualified GHC.TypeLits as TL
import ChessTypes
import FirstClassFunctions
import Lib

-- kholmovVsBronstein = chess
--     pawn e2 to e4
--     pawn c7 to c5
--     knight g1 to f3
--     knight g8 to f6
--     knight b1 to c3
--     pawn d7 to d6 -- end  -- Successfully compiles, with <= 22GB memory usage.
--     pawn d2 to d4 -- end
--     pawn c5 to d4 -- end  -- FIXME: Too much memory usage.
--     knight f3 to d4
--     pawn a7 to a6 -- end  -- Finishes compiling in about 3 minutes!
--     bishop c1 to g5 -- end
--     pawn e7 to e6 end  -- FIXME: Crashes after 2m57
    -- pawn f2 to f4
    -- bishop f8 to e7
    -- queen d1 to f3
    -- queen d8 to c7
    -- king e1 to c1
    -- knight b8 to d7
    -- pawn g2 to g4  -- Move 10 - absolutely tanks compiler
    -- pawn b7 to b5
-- end

-- kholmovVsBronsteinShort = chess
--     p e4 p c5
--     n f3 n f6
--     n c3 p d6
--     p d4 p d4
--     n d4 p a6
--     b g5 p e6 end  -- Successfully compiles in 1m44 - WHAT
    -- p f4 b e7 end -- FIXME: Also crashes
