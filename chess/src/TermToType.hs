module TermToType where

import Data.Proxy(Proxy(..))
import ChessTypes
import GHC.TypeLits
import Data.Singletons
import qualified Data.Singletons.TypeLits as TL
import Data.Singletons.TH

-- Constraints for transforming term-level values to type-level, erm, types

-- Use singletons where it doesn't throw horrible errors
$(genSingletons [''PieceName])
$(genSingletons [''Team])

-- -- FIXME: Nasty errors, try and go through this
-- -- 1. Could not deduce: Demote FirstClassFunctions.MyNat ~ FirstClassFunctions.MyNat
-- -- 2. Couldn't match expected type ‘FirstClassFunctions.MyNat’ with actual type ‘Demote FirstClassFunctions.MyNat’
-- $(genSingletons [''PieceInfo])

-- -- FIXME: Nasty errors again
-- -- Couldn't match type ‘Symbol’ with ‘text-1.2.4.0:Data.Text.Internal.Text’
-- --     Expected type: Symbol
-- --     Actual type: Demote Symbol
-- $(genSingletons [''Position])

data MyPosition where
    At :: TL.Symbol -> TL.Nat -> MyPosition

$(genSingletons [''MyPosition])

data MyPieceInfo where
    Info :: TL.Nat -> MyPosition -> Bool -> MyPieceInfo

$(genSingletons [''MyPieceInfo])
