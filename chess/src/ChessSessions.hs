{-# LANGUAGE RebindableSyntax #-}

module ChessSessions where

import Control.SessionTypes
import Control.SessionTypes.Indexed

import Data.Proxy

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
