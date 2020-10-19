module TermToType where

import Data.Proxy(Proxy(..))
import ChessTypes
import FirstClassFunctions
import GHC.TypeLits
import Data.Singletons
import qualified Data.Singletons.TypeLits as TL
import Data.Singletons.TH
import Data.Singletons.Prelude.Bool

-- Constraints for transforming term-level values to type-level, erm, types

-- Use singletons where it doesn't throw horrible errors
$(genSingletons [''PieceName])
$(genSingletons [''Team])
$(genSingletons [''MyNat])

$(genSingletons [''Column])

data MyPosition where
    At :: Column -> MyNat -> MyPosition

$(genSingletons [''MyPosition])

data MyPieceInfo where
    Info :: MyNat -> MyPosition -> Bool -> MyPieceInfo

$(genSingletons [''MyPieceInfo])

data MyPiece where
    MkPiece :: Team -> PieceName -> MyPieceInfo -> MyPiece

$(genSingletons [''MyPiece])
