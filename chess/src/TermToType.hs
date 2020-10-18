module TermToType where

import Data.Proxy(Proxy(..))
import ChessTypes
import GHC.TypeLits
import Data.Singletons
import Data.Singletons.TH

-- Constraints for transforming term-level values to type-level, erm, types

-- Use singletons where it doesn't throw horrible errors
$(genSingletons [''PieceName])
$(genSingletons [''Team])
