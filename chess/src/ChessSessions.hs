{-# LANGUAGE RebindableSyntax #-}

module ChessSessions where

import Control.SessionTypes hiding (Nat(..))
import Control.SessionTypes.Indexed

import Data.Proxy
import Data.Singletons
import Data.Type.Nat hiding (SNat(..))

import MakeSingletons
import ChessTypes

prog :: Monad m => STTerm m ('Cap '[] (Int :?> Sel '[Bool :!> r, String :!> r])) ('Cap '[] r) ()
prog = do
  x <- recv
  case x of
    0 -> sel1 >> send True
    n -> sel2 >> sel1 >> send "False"

progDual :: Monad m => STTerm m ('Cap '[] (Int :!> Off '[Int :?> r, String :?> r])) ('Cap '[] r) ()
progDual = do
  send 0
  (recv >> return ()) <&> (recv >> return ())

sendChar :: Monad m => STTerm m ('Cap '[] ('R (Char :!> Off '[V, Wk Eps]))) ('Cap '[] Eps) ()
sendChar = recurse go
  where
    go = do
      send 'c'
      offS (var0 >> go) (offZ $ (weaken0 >> eps0))

prog_recursion :: MonadSession m => m ('Cap ctx (R (Int :?> Sel '[Wk Eps, V]))) ('Cap ctx Eps) Int
prog_recursion = recurseFix $ \f -> do
  x <- recv
  if x < 10
    then selN2 >> f
    else sel1 >> weaken0 >> eps x

prog_recursion_dual :: MonadSession m => m ('Cap ctx (R (Int :!> Off '[Wk Eps, V]))) ('Cap ctx Eps) Int
prog_recursion_dual = recurse $ go 0
    where 
      go n = do
        send n
        (weaken0 >> eps n) <&> (var $ go (n + 1))

-- -- recurse :: MonadSession m => m ('Cap (s : ctx) s) r a -> m ('Cap ctx ('R s)) r a
-- chess_recursion :: SPosition pos1 -> SPosition pos2
--     -> STTerm m ('Cap ctx (R ((SPosition pos1, SPosition pos2) :!> (SPosition pos3, SPosition pos4) :?> Off '[V, Wk Eps])))
--         ('Cap ctx Eps) ()
-- chess_recursion x y = recurse $ f x y
--     where
--         f from to = do
--             send (from, to)
--             pair <- recv
--             (var0 >> uncurry f pair) <&> (weaken0 >> eps0)
--             return ()

-- recurse :: MonadSession m => m ('Cap (s : ctx) s) r a -> m ('Cap ctx ('R s)) r a
chess_recursion :: MonadSession m => Position -> Position
    -> m ('Cap '[] (R ((Position, Position) :!> (Position, Position) :?> Off '[V, Wk Eps])))
         ('Cap '[] Eps) ()
chess_recursion x y = recurse $ f x y
    where
        f from to = do
            send (from, to)
            pair <- recv
            (var0 >> uncurry f pair) <&> (weaken0 >> eps0)
            return ()

-- sendPos :: MonadSession m => SPosition pos1 -> m ('Cap '[] ((SPosition pos1) :!> Eps)) ('Cap '[] Eps) ()
-- sendPos x = do
--     send x
--     return ()

-- proxToSPos :: Proxy (pos1 :: Position) -> SPosition pos1
-- proxToSPos (Proxy :: Proxy (At A Nat1)) = _a1
-- proxToSPos (Proxy :: Proxy (At A Nat2)) = _a2
-- proxToSPos (Proxy :: Proxy (At A Nat3)) = _a3
-- proxToSPos (Proxy :: Proxy (At A Nat4)) = _a4
-- proxToSPos (Proxy :: Proxy (At A Nat5)) = _a5
-- proxToSPos (Proxy :: Proxy (At A Nat6)) = _a6
-- proxToSPos (Proxy :: Proxy (At A Nat7)) = _a7
-- proxToSPos (Proxy :: Proxy (At A Nat8)) = _a8
-- proxToSPos (Proxy :: Proxy (At B Nat1)) = _b1
-- proxToSPos (Proxy :: Proxy (At B Nat2)) = _b2
-- proxToSPos (Proxy :: Proxy (At B Nat3)) = _b3
-- proxToSPos (Proxy :: Proxy (At B Nat4)) = _b4
-- proxToSPos (Proxy :: Proxy (At B Nat5)) = _b5
-- proxToSPos (Proxy :: Proxy (At B Nat6)) = _b6
-- proxToSPos (Proxy :: Proxy (At B Nat7)) = _b7
-- proxToSPos (Proxy :: Proxy (At B Nat8)) = _b8
-- proxToSPos (Proxy :: Proxy (At C Nat1)) = _c1
-- proxToSPos (Proxy :: Proxy (At C Nat2)) = _c2
-- proxToSPos (Proxy :: Proxy (At C Nat3)) = _c3
-- proxToSPos (Proxy :: Proxy (At C Nat4)) = _c4
-- proxToSPos (Proxy :: Proxy (At C Nat5)) = _c5
-- proxToSPos (Proxy :: Proxy (At C Nat6)) = _c6
-- proxToSPos (Proxy :: Proxy (At C Nat7)) = _c7
-- proxToSPos (Proxy :: Proxy (At C Nat8)) = _c8
-- proxToSPos (Proxy :: Proxy (At D Nat1)) = _d1
-- proxToSPos (Proxy :: Proxy (At D Nat2)) = _d2
-- proxToSPos (Proxy :: Proxy (At D Nat3)) = _d3
-- proxToSPos (Proxy :: Proxy (At D Nat4)) = _d4
-- proxToSPos (Proxy :: Proxy (At D Nat5)) = _d5
-- proxToSPos (Proxy :: Proxy (At D Nat6)) = _d6
-- proxToSPos (Proxy :: Proxy (At D Nat7)) = _d7
-- proxToSPos (Proxy :: Proxy (At D Nat8)) = _d8
-- proxToSPos (Proxy :: Proxy (At E Nat1)) = _e1
-- proxToSPos (Proxy :: Proxy (At E Nat2)) = _e2
-- proxToSPos (Proxy :: Proxy (At E Nat3)) = _e3
-- proxToSPos (Proxy :: Proxy (At E Nat4)) = _e4
-- proxToSPos (Proxy :: Proxy (At E Nat5)) = _e5
-- proxToSPos (Proxy :: Proxy (At E Nat6)) = _e6
-- proxToSPos (Proxy :: Proxy (At E Nat7)) = _e7
-- proxToSPos (Proxy :: Proxy (At E Nat8)) = _e8
-- proxToSPos (Proxy :: Proxy (At F Nat1)) = _f1
-- proxToSPos (Proxy :: Proxy (At F Nat2)) = _f2
-- proxToSPos (Proxy :: Proxy (At F Nat3)) = _f3
-- proxToSPos (Proxy :: Proxy (At F Nat4)) = _f4
-- proxToSPos (Proxy :: Proxy (At F Nat5)) = _f5
-- proxToSPos (Proxy :: Proxy (At F Nat6)) = _f6
-- proxToSPos (Proxy :: Proxy (At F Nat7)) = _f7
-- proxToSPos (Proxy :: Proxy (At F Nat8)) = _f8
-- proxToSPos (Proxy :: Proxy (At G Nat1)) = _g1
-- proxToSPos (Proxy :: Proxy (At G Nat2)) = _g2
-- proxToSPos (Proxy :: Proxy (At G Nat3)) = _g3
-- proxToSPos (Proxy :: Proxy (At G Nat4)) = _g4
-- proxToSPos (Proxy :: Proxy (At G Nat5)) = _g5
-- proxToSPos (Proxy :: Proxy (At G Nat6)) = _g6
-- proxToSPos (Proxy :: Proxy (At G Nat7)) = _g7
-- proxToSPos (Proxy :: Proxy (At G Nat8)) = _g8
-- proxToSPos (Proxy :: Proxy (At H Nat1)) = _h1
-- proxToSPos (Proxy :: Proxy (At H Nat2)) = _h2
-- proxToSPos (Proxy :: Proxy (At H Nat3)) = _h3
-- proxToSPos (Proxy :: Proxy (At H Nat4)) = _h4
-- proxToSPos (Proxy :: Proxy (At H Nat5)) = _h5
-- proxToSPos (Proxy :: Proxy (At H Nat6)) = _h6
-- proxToSPos (Proxy :: Proxy (At H Nat7)) = _h7
-- proxToSPos (Proxy :: Proxy (At H Nat8)) = _h8
-- proxToSPos (Proxy :: Proxy x)           = undefined

strToPos :: String -> Position
strToPos "a1" = At A 1
strToPos "a2" = At A 2
strToPos "a3" = At A 3
strToPos "a4" = At A 4
strToPos "a5" = At A 5
strToPos "a6" = At A 6
strToPos "a7" = At A 7
strToPos "a8" = At A 8
strToPos "b1" = At B 1
strToPos "b2" = At B 2
strToPos "b3" = At B 3
strToPos "b4" = At B 4
strToPos "b5" = At B 5
strToPos "b6" = At B 6
strToPos "b7" = At B 7
strToPos "b8" = At B 8
strToPos "c1" = At C 1
strToPos "c2" = At C 2
strToPos "c3" = At C 3
strToPos "c4" = At C 4
strToPos "c5" = At C 5
strToPos "c6" = At C 6
strToPos "c7" = At C 7
strToPos "c8" = At C 8
strToPos "d1" = At D 1
strToPos "d2" = At D 2
strToPos "d3" = At D 3
strToPos "d4" = At D 4
strToPos "d5" = At D 5
strToPos "d6" = At D 6
strToPos "d7" = At D 7
strToPos "d8" = At D 8
strToPos "e1" = At E 1
strToPos "e2" = At E 2
strToPos "e3" = At E 3
strToPos "e4" = At E 4
strToPos "e5" = At E 5
strToPos "e6" = At E 6
strToPos "e7" = At E 7
strToPos "e8" = At E 8
strToPos "f1" = At F 1
strToPos "f2" = At F 2
strToPos "f3" = At F 3
strToPos "f4" = At F 4
strToPos "f5" = At F 5
strToPos "f6" = At F 6
strToPos "f7" = At F 7
strToPos "f8" = At F 8
strToPos "g1" = At G 1
strToPos "g2" = At G 2
strToPos "g3" = At G 3
strToPos "g4" = At G 4
strToPos "g5" = At G 5
strToPos "g6" = At G 6
strToPos "g7" = At G 7
strToPos "g8" = At G 8
strToPos "h1" = At H 1
strToPos "h2" = At H 2
strToPos "h3" = At H 3
strToPos "h4" = At H 4
strToPos "h5" = At H 5
strToPos "h6" = At H 6
strToPos "h7" = At H 7
strToPos "h8" = At H 8
strToPos _    = At A Z
