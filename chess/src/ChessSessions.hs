module ChessSessions where

import Control.SessionTypes
import Control.SessionTypes.Indexed

import Data.Proxy

import MakeSingletons
import ChessTypes

prog :: Monad m => STTerm m s s Team
prog = return White

prog2 :: Monad m => STTerm m ('Cap '[] (Team :!> Team :?> Eps)) ('Cap '[] Eps) Team
prog2 = send Black >> recv >>= eps

prog3 :: Monad m => STTerm m s s (Proxy StartDec)
prog3 = return (Proxy @StartDec)