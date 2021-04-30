module BenchmarkGames where

import FlatBuilders
import MakeProxies
import qualified GHC.TypeLits as TL
import ChessTypes
import FirstClassFunctions
import Chesskell

-- /usr/bin/time -v stack ghc -- src/BenchmarkGames.hs -freduction-depth=0

-- kholmovVsBronsteinShort = chess
--     p e4 p c5
--     n f3 n f6
--     n c3 p d6 -- 13.1 GB, 1:41
--     p d4 p d4 -- end  -- 15.2 GB, 2:07
--     n d4 p a6 end  -- 24.4 GB, 2:59

-- rubinsteinVsVidmar = chess
--     p d4 n f6
--     p c4 p e5
--     p e5 n g4 -- end  -- 12.4 GB, 1:27
--     b f4 n c6 -- end  -- 24.8 GB, 3:27
--     n f3 b b4 end -- 25.9 GB, 6:10

-- lauVsBastian = chess
--     p e4 p e5
--     n f3 n c6
--     b b5 b c5 end -- Crashes after 4:38.

-- xiangVsZhou = chess
--     p d4 n f6
--     p c4 p e6
--     n f3 p b6 -- end  -- 13.0 GB, 1:31
--     p g3 b b7 -- end  -- 25.0 GB, 3:02
--     b g2 b e7 end -- 25.8GB, 4:28

-- zunigaVsMarch = chess
--     p e4 p c5 
--     n f3 p d6 
--     p d4 p d4 -- end -- 12.9GB, 1:32
--     n d4 n f6 -- end -- 15.2 GB, 2:05
--     n c3 n c6 end  -- 22.8 GB, 2:48
