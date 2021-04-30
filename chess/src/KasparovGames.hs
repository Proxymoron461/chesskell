module KasparovGames where

import FlatBuilders
import MakeProxies
import qualified GHC.TypeLits as TL
import ChessTypes
import FirstClassFunctions
import Chesskell

-- SUCCESS!
-- game = chess p e4 p c5 n f3 p d6 b b5 n d7 p d4 p d4 end -- q d4 p a6 end --b d7 b d7 n c3 
-- p e5 q d3 p h6 n d2 b e6 n c4 r c8 n e3 n f6 O-O b e7 r d1 O-O 
-- b d2 r c5 p a4 q c8 b e1 r d8 p b4 r c7 p a5 b c4 q d2 p d5 
-- n d5 n d5 d5 b g5 p d6 r d7 q c3 b e3 q e3 r d6 r d6 
-- r d6 q e5 q c6 q c5 q c5 c5 r e6 b c3 b d5 r e1 r e1 
-- b e1 p f6 

-- SUCCESS! (Thanks to less expensive AnySpaceInCheck.)
-- game = chess p e4 p c5 n f3 n c6 b b5 p g6 k g1 b g7 b c6 pawn b7 to c6 end --r e1 n f6 p e5 n d5
-- p c4 n c7 p d4 p d4 q d4 n e6 q h4 p d6 d6 q d6 n c3 p h5 
-- b d2 O-O n e4 q c7 b c3 p f6 q g3 q g3 g3 r d8 b a5 r d3 
-- r d1 r d1 r d1 k f7 n d4 n d4 r d4 b e6 p b3 b f8 n c5 b f5 
-- r d1 k e8 n a6 r c8 b b4 p c5 b a5 p e5 n c7 k f7 n b5 r a8 
--  k f1 p a6 n d6 k e6 n b7 b e7 n d8 b d8 b d8 b b1 b c7 b a2 
-- r d6 k e7 r b6 p a5 b d6 k f7 b c5 p a4 a4 b c4 k g1 
-- r a4 r b7 k e6 r b6 k f7 r b7 k e6 r b6 k f5 r b4 r a1 
-- k h2 b d5 r b6 r c1 b e3 r c2 r b4 p g5 r a4 p g4 r a5 b e4 r c5 
-- r a2 r c8 


-- SUCCESS!
-- game = chess p d4 n f6 n f3 p g6 p g3 b g7 b g2 k g8 end -- k g1 d6 r e1 n d7 p e4 p e5 
--  n c3 r e8 p a4 p a5 b e3 p b6 p h3 b b7 n h2 r b8 e5 n e5 r b1 
-- n c4 b d4 p c5 b f6 b f6 q d3 n e5 q d2 n c4 q d3 n e5 q d2 

-- SUCCESS!
-- game = chess p d4 n f6 b f4 p c5 p c5 n c6 n c3 p e5 end -- b g5 b c5 end --p e3 b b4 n e2 p d5 
-- p a3 b c3 n c3 b e6 b b5 p d4 b f6 q f6 n e4 q g6 n g3 O-O 
-- q d3 q d3 b d3 p f5 O-O-O e3 e3 p a6 n e2 p e4 n f4 
-- p d3 n e6 p c2 k c2 r f7 r e1 r e8 n f4 r e7 n d5 r e5 
-- r d3 k f7 p g3 p g5 r d1 r d8 p b4 r e8 r c3 r e6 r c5 r e5 r d3
-- r d7 n c3 r d3 k d3 r c5 c5 k e6 p e4 n e5 k e2 n d7 
-- p f5 k f5 k d3 n c5 k d4 n d7 n e4 k g4 p h3 k h3 n g5 
-- k g3 

-- SUCCESS! (But only 6 moves??? Uh oh)
-- game = chess p e4 p c5 n f3 p d6 b b5 b d7 end -- b d7 q d7 end -- p c4 n c6 end --n c3 p g6 p d4 b g7
-- b e3 p d4 n d4 n f6 p f3 O-O O-O a6 n a4 r b8 n c6 q c6 
-- n b6 n d7 n d7 q d7 r f2 p b5 p c5 q a7 r c1 p c5 b c5 q c7 
-- p f4 r d8 q e1 r d3 p e5 q d7 p h3 q e6 r d2 r d2 q d2 p f6 
-- p  f6 b f6 p b4 r e8 r f1 k f7 r f3 p h5 p a3 q c4 q d7 p a5 p f5 
-- p g5 q e6 q e6 e6 k e6 a5 r a8 b b6 k d5 r f5 k c4 
-- r c5 k b3 r b5 k a3 k f2 k a4 r f5 k b3 k e3 k c4 k e4 p g4 
--  r h5 p h3 h3 b c3 r c5 k b3 p h4 b b4 r g5 k c4 p h5 b d6 
-- p  h6 

-- SUCCESS!
-- game = chess p e4 p c5 n c3 p d6 knight c3 to e2 n f6 p g3 p g6 b g2 n c6 end --p d3 b g7 O-O O-O 
-- p  a3 r b8 r b1 p b6 p b4 b b7 p h3 n d7 b e3 r e8 q d2 b a8 p f4 r c8
-- p g4 p e6 p f5 n d4 b d4 p d4 n b5 p f5 f5 n e5 n d4 p d5 
-- q f4 p e4 e4 q c7 p f6 b f8 r b3 n c4 r c3 p a6 q c7 r c7 
-- n f4 p b5 n d5 r c8 r d3 b d6 n b3 b d5 d5 r e5 n c5 p a5 
-- r d1 p a4 r f1 r e8 r f3 p h5 n d3 r e2 r 3f2 r 2e3 r f3 r e2
-- r 1f2 k h7 b f1 r 2e4 n c5 r e1 k g2 n e3 k h1 n d5 r d3 
-- r 8e5 n d7 r g5 r f3 b c7 n c5 p h4 r d4 b g3 n d3 r a1 r e4 
-- r a3 r e7 k g8 r e8 k h7 r f8 r a1 r f7 k g8 r g7 k f8 
-- r a7 r f5 r f5 p f5 k g2 n e3 k f3 n f1 n c5 r e1 k g2 n e3 
-- k f3 n c4 r a6 n d2 k g2 r e2 k h1 r h2 

-- SUCCESS!
-- game = chess p e4 p c5 n f3 p d6 p d4 p d4 n d4 n f6 n c3 p a6 end --p h3 p e6 p g4 p h6 
-- b g2 p g5 b e3 n d7 q e2 n e5 O-O-O n d7 p h4 r g8 g5 p g5 
--  k b1 p b5 p a3 b b7 b c1 r c8 r h3 n g6 b h1 n e5 r g3 b e7 
-- n a2 r h8 r c3 r c3 n c3 q c7 b g2 q c4 q c4 p c4 p f3 r h2 
--  b f1 n f4 b e3 b d8 r d2 r d2 b d2 b b6 b f4 p f4 n e2 p d5 
-- p d5 b d5 b g2 n g4 n f4 n e3 n e2 n g2 n g2 b f3 
-- n f4 k e7 k c1 p e5 n h4 b e3 k b1 b f4 n f3 k e6 p b3 p e4 
-- n d4 k d5 p c3 b e5 c4 k c4 n f5 k c3 k c1 k d3 k d1 p e3 

-- SUCCESS!
-- game = chess p b3 p e5 b b2 n c6 p e3 p g6 p c4 b g7 n f3 p d6 end --b e2 n f6 p d4 O-O 
-- O-O d4 n d4 r e8 n c6 p c6 b f3 b d7 n c3 r b8 p h3 p c5 
-- q d2 p h5 r d1 q e7 r e1 p a6 q c1 b e6 n e2 n d7 b g7 k g7 
-- n f4 n e5 b e2 q f6 b h5 r h8 b e2 p g5 n e6 p e6 p f4 p f4 
-- p  f4 n c6 r d3 n d4 r g3 k f7 b d3 r g8 r g5 p e5 r f1 k e7 
--  q e3 k d7 r g8 r g8 e5 q e5 r f7 k e6 q f2 q g5 r c7 
-- q c1 k h2 

-- FIXME: One of these moves results in check??
-- game = chess p d4 n f6 p c4 p e6 n c3 b b4 q c2 k g8 p a3 b c3 end --q c3 p d5 n f3 
-- p c4 q c4 p b6 p h4 b b7 b g5 q d5 r c1 n d7 q d5 b d5 n e5 
-- p c5 b f6 n f6 c5 p c5 p f3 r b8 p e4 b a2 r c2 b b1 r d2 
-- r d8 r d8 r d8 b b5 n h5 p g4 n f4 k f2 p f6 n c6 r d2 k e3
-- r b2 k f4 r b5 r d1 p h6 p h5 r b3 p e5 b d3 p a4 p c4 n a7 r a3
-- n b5 r a4 k e3 p e5 n d6 r a3 k f2 k f8 n b5 r b3 

-- game = chess p d4 n f6 p c4 p e6 n c3 b b4 q c2 k g8 end -- p a3 b c3 end --q c3 p d6 b g5 
-- n d7 n f3 p b6 p e3 b b7 n d2 r c8 b d3 p c5 O-O h6 b h4 p d4 
-- p d4 p d5 p b3 q c7 r e1 n h5 p f3 r e8 r c1 p c4 c4 p e5 
-- b f5 p d4 q d4 b c6 n e4 q f4 b d7 b d7 b g3 n g3 g3 
-- q c7 n d6 r e1 r e1 q c5 q c5 r c5 r e7 b e6 r a7 r c6 
-- n e4 b c4 p g4 b e6 k f2 k h7 p a4 r c2 k g3 r a2 n d6 b b3 
-- r a6 r a4 r b6 b e6 n e4 r a7 

-- game = chess p d4 n f6 p c4 p e6 n c3 b b4 q c2 k g8 end -- p a3 b c3 end --q c3 p d5 n f3 
-- p c4 q c4 p b6 p h4 b b7 b g5 n d7 n e5 p h6 n d7 q d7 b f6 
-- p f6 r h3 p f5 r d1 k h7 q c1 b d5 r c3 p c6 p g3 p a5 p e3 r c8 
-- b e2 q e7 r d2 r c7 b c4 r c8 r c2 b e4 b d3 b f3 b e2 b e4 
-- b d3 b f3 b e2 

-- game = chess p e4 p c5 n f3 p d6 b b5 n d7 p d4 p d4 end -- q d4 n f6 end --n c3 p e5 q d3 p h6 
-- b e3 p a6 b c4 b e7 p a4 q c7 O-O O-O n d2 n b6 b b6 q b6 
-- p a5 q c7 n d5 n d5 b d5 b e6 n c4 k h8 p c3 r d8 r d1 b g5 
-- n b6 b g4 p f3 b e6 p g3 p h5 p h4 b h6 n c4 b c8 n b6 b e6 n c4 
-- b c8 n b6 

-- game = chess p e4 p e5 p f4 p f4 n c3 q h4 k e2 q d8 end -- p d4 n f6 end --b f4 b b4 b g5 
-- b c3 c3 p d6 n f3 O-O b f6 q f6 k f2 p c5 p h3 r e8 q d3 p b6 
-- r e1 b b7 p g3 n c6 b g2 r c8 p a3 n b8 r e3 n d7 r f1 q e7 
--  k g1 b e4 q e2 p d5 n h4 p g6 b e4 p e4 q g4 n f6 q g5 n h5 
-- q g4 n f6 q g5 n h5 q g4 n g7 q e2 p d4 d4 p f5 n g2 q d6 
-- p c3 q a3 q d2 n e6 p g4 n g5 f5 n f3 r f3 p f3 r f3 r f8 
-- n h4 q e7 q f2 q g5 r g3 q c1 k g2 r c3 n g6 r g3 q g3
-- q d2 k h1 q d1 k g2 q e2 k g1 q d1 k g2 q c2 k g1 q b1 
-- k h2 q b2 k h1 q b1 k h2 q a2 k g1 q a1 k h2 q b2 k h1 q c1 
-- k h2 q d2 k h1 q d1 k h2 q e2 k g1 q d1 k h2 q d2 k h1 
-- q d1 

-- game = chess p e4 p e5 n f3 n c6 p d4 p d4 n d4 b c5 end -- b e3 q f6 end --p c3 n e7 p g3 p d5 
-- b g2 p e4 O-O O-O n d2 b b6 n e4 q g6 r e1 r d8 n c6 n c6 
-- q a4 b e6 p h4 p h6 q b5 p a6 q a4 n e5 b b6 p b6 n c5 p c5 
-- r e5 r d2 q f4 r b2 r c5 r d8 r e1 q f6 q f6 p f6 p a4 
-- r d2 r f1 p b5 b5 p b5 r c6 k g7 r c7 b c4 r e1 r f2 b c6
-- r c2 

-- game = chess p f4 p e6 n f3 p d5 p e3 p c5 p b3 p f6 end -- b d3 p g6 end --q e2




-- game = chess p d4 n f6 p c4 p e6 n c3 b b4 end -- q c2 p d5 end -- d5 q d5 n f3 q f5 q b3 n c6
-- b d2 O-O p h3 p a5 p g4 q g6 p a3 p a4 q c4 b c3 b c3 n d5 b d2
-- p f6 r c1 b d7 q c2 q c2 r c2 b e8 p e3 b g6 r c5 b e4 b e2 p e5
-- O-O d4 n d4 n d4 d4 p c6 r e1 b g6 b f1 b f7 p f4 r d8
-- p f5 r d7 r a5 r a5 b a5 p g6 g6 p g6 b d2 p g5 b d3 k g7 
-- b c2 p b5 b f5 r d8 k f2 b g6 b e6 b f7 b f5 b g6 b e6 n f4 
--  b f4 p f4 r d1 b e4 p h4 k g6 r e1 b c2 r c1 b e4 r e1 b c2 


-- game = chess p c4 p e6 n c3 p d5 p d4 n f6 b g5 b e7 end -- p e3 p h6 end --b h4 O-O q c2 p c5 
-- p c5 n d7 d5 n d5 b e7 q e7 n d5 p d5 n f3 n c5 b e2 
-- b g4 O-O r c8 r d1 r d8 p h3 n e6 q a4 b h5 r d2 p d4 q a7 
-- b f3 b f3 q b4 r d1 r c1 r c1 q d2 r d1 q b2 q b7 q a2 
-- q b1 q b1 r b1 p e3 e3 n g5 p h4 n f3 f3 

-- game = chess p d4 n f6 p c4 p g6 n c3 p d5 n f3 b g7 end -- p h4 p c6 end --b g5 p c4 p e4 b e6 
-- p e5 n d5 p h5 n d7 p h6 b f8 n e4 p f6 b d2 p b5 p a4 b f5 n g3 p e6 
-- n f5 p f5 b5 p b5 b e2 b e7 O-O a5 p b3 p c3 b b5 p d2
-- p e6 O-O d7 n c3 b c4 k h8 q d2 n e4 q e3 q d7 n d2 
-- n d2 q d2 b b4 q d3 q d6 r a2 r e8 r e2 q f4 p g3 q h6 k g2
-- p f4 r h1 q g5 r e8 r e8 b f7 r e3 e3 q g3 k f1 p f3 

-- game = chess p e4 p e5 n f3 n c6 b b5 n f6 n c3 n d4 end -- b a4 b c5 end --n e5 O-O n d3 
-- b b6 p e5 n e8 n d5 p c6 n e3 p d5 O-O f6 p c3 n f5 b c2 q e7 
-- p f4 n e3 e3 b f5 p a4 b c7 p b3 p e5 b a3 b d6 b d6 n d6 
-- n e5 b c2 q c2 n f7 n g4 p h5 n f2 q e3 p g3 r e8 q g6 q f3 
--  r e1 r e1 r e1 n h6 q d3 q d3 n d3 n f5 p a5 k f7 n e5 k g8 
-- p b4 r e8 k f2 p a6 n d7 r e1 k e1 k f7 k e2 k e7 n c5 n d6 
-- p h3 k f6 p g4 p g4 g4 p g5 k f3 k g6 p f5 k f6 k e3 n c4 
-- k d4 n d6 n d7 k e7 n c5 k f6 k d3 k e5 k e3 k f6 k d4 n b5 
-- k d3 n d6 k e3 n c4 k d4 n d6 n e6 n e4 n d8 n d6 k c5 n e4 
-- k b6 n c3 n b7 n a2 k a6 n b4 k b6 p d4 n c5 p d3 n d3 
-- n d3 p a6 

-- game = chess p d4 n f6 n f3 p g6 b f4 p d6 p e3 n h5 end -- b g5 p h6 end --b h4 p g5 n d2 n g7 
--  b g3 n f5 p c3 b g7 b d3 p e6 n a3 n d7 q f3 n g3 g3 p a6 
-- O-O-O r b8 n c2 p b5 q e2 p c5 p f4 p d5 c5 n c5 p e4 p b4 b4
-- n d3 q d3 O-O d5 q d5 q d5 p d5 n f3 p a5 r d5 p b4 
-- p g5 p h5 n d4 b b7 r d6 r c8 k d2 r d8 r d8 r d8 k e3 
-- r e8 k f2 r a8 p a3 p a3 a3 r a3 r h5 r a2 k g1 b d4 
--  n d4 r g2 k f1 r g3 k f2 r g4 k e3 r e4 k d3 r e5 r h2 r g5
-- r b2 b d5 k e3 r e5 k f2 k g7 r b5 p f6 r a5 k g6 n e2 r f5 
-- k e3 r e5 k f2 k g5 n c3 b e6 r e5 p e5 k e3 

-- game = chess p d4 n f6 p c4 p g6 n c3 p d5 b g5 b g7 end -- b f6 b f6 end --d5 p c6 p e4 O-O 
-- n f3 p d5 p e5 b g7 q d2 n c6 b b5 b g4 n g1 p f6 p h3 b e6 
-- p f6 r f6 b c6 p c6 n e2 q d6 O-O r f8 r e1 b c8 n a4 p e5 
-- p e5 q e5 p b4 r e6 n c5 r e8 n b3 q b2 q b2 b b2 n d4 
-- r e1 r e1 b d7 r e2 b c3 r c2 b b4 n c6 b d6 n a7 r e8 
-- p g4 p h5 p f3 r e1 k g2 k f7 n c6 p h4 n d4 r d1 r d2 r d2 
-- n d2 k f6 k f2 k e5 k e3 p g5 p f4 p f4 k d3 b e7 n 2f3 k d6 
-- n f5 b f5 f5 k c5 n d4 k d6 p a4 b d8 n e6 b b6 n f4 
-- k e5 p f6 b c5 p f7 k f6 n d5 k f7 k e4 k e6 n f4 k d6 n g2 
-- b f2 k f3 b g3 k g4 k c5 n h4 b f2 n f5 k b4 n g3 k a4 p h4 
-- k b5 p h5 b e3 n e4 k c6 n g5 k d7 p h6 k e7 p h7 b d4 k f5 b a1 
-- k g6 b b2 n f7 b a1 n h6 b h8 n g4 b a1 n e3 b h8 n d5 k e6 
-- n f4 k e7 n h3 k e6 n g5 k e7 n f7 b a1 p h8 b h8 

-- game = chess p c4 p c5 p g3 p g6 b g2 b g7 p e3 p h5 end -- p d4 p h4 end --n c3 n c6 p d5 b c3 
-- p c3 n e5 n f3 n f3 b f3 p d6 p g4 p h3 p g5 q a5 b d2 r h4 p e4 
-- p e5 r g1 n e7 q b3 q c7 q c2 b d7 b e2 n c8 p a4 p a5 p f3 n b6 
-- k f2 O-O-O q b3 k b8 r g3 r h8 k g1 k a7 k h1 b e8 r g1 
-- b d7 r b1 b e8 r b2 n d7 r b1 p b6 r g1 q d8 r b1 r f4 q c2 
-- r h4 b e1 p f6 b h4 r h4 f6 q f6 q d2 q f4 q f4 p f4 
-- r b2 k b7 r b1 k c7 k g1 r h5 k f2 r g5 b f1 r h5 k e1 r h4 
-- k d1 r h5 k d2 r h4 k e2 r h5 k f2 r h4 b e2 r h5 r g1 n e5 
-- r a2 b d7 r a1 b e8 k e1 b d7 k d2 b e8 k c2 b d7 k b3 k d8 
-- k b2 k c7 k b3 k d8 r f1 k c7 r g4 b g4 g4 r h7 r f4 r f7 
-- r f7 n f7 k c2 k d7 b f1 n g5 k d3 k e7 k e3 k f6 k f4 n f7
-- b h3 p g5 k g3 n e5 b f1 n g6 b e2 

-- game = chess p e4 p c5 n f3 p d6 p d4 p d4 n d4 n f6 end -- n c3 p a6 end --p h3 p e6 p g4 n d7 
--  b e3 p b5 p a3 b b7 p g5 n b6 p h4 n 8d7 q d2 r c8 O-O-O d5 d5
-- p b4 b4 b d5 n d5 n d5 b a6 b b4 q e2 O-O b c8 q c8 
--  n b3 q c6 r d5 p d5 r d1 n e5 b c5 b c5 q e5 b f2 q d5 
-- q d5 r d5 b h4 p c4 p f5 f6 b f6 n c5 r c8 p b4 b e7 k d2 
-- b c5 c5 k f7 

-- game = chess p e4 p c5 n f3 p d6 p d4 p d4 n d4 n f6 end -- n c3 p a6 end --b e2 p e6 O-O b e7 
-- p a4 n c6 b e3 O-O q d2 q c7 r d1 b d7 p f3 n a5 q e1 r c8 
-- k h1 r e8 b f1 n c4 b c4 q c4 p a5 q b4 n b3 p h6 q g3 n h5 
-- q f2 n f6 b b6 p e5 q d2 q c4 b f2 b e6 b h4 r d8 q e1 q c6 
-- r c1 q e8 b f2 r c6 n d5 r c8 n a1 b d8 q b4 b d5 d5 r c4 
-- q b7 p e4 r e1 b a5 p c3 q b5 q b5 p b5 n b3 b d8 n d2 r a4 
-- n e4 n e4 r e4 b f6 r e1 k h7 b g3 r a2 r b4 r c5 b d6 
-- r d5 b f4 b e7 r e4 b f6 r b4 b e7 r e4 b f6 

-- game = chess p e4 p d5 p e5 p c5 p b3 p e6 p f4 end -- p f5 n f3 end
