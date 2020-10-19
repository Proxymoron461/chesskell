module TermToType where

import Data.Proxy(Proxy(..))
import ChessTypes
import FirstClassFunctions
import qualified GHC.TypeLits as TL
import Data.Singletons
import qualified Data.Singletons.TypeLits as TL
import Data.Singletons.TH
import Data.Singletons.Prelude.Bool
import Data.Type.Nat hiding (SNat(..))

-- Constraints for transforming term-level values to type-level, erm, types

-- Transform various types into singleton types (for use at the value-level)
$(genSingletons [''PieceName])
$(genSingletons [''Team])
$(genSingletons [''Nat])

$(genSingletons [''Column])

data MyPosition where
    At :: Column -> Nat -> MyPosition

$(genSingletons [''MyPosition])

data MyPieceInfo where
    Info :: Nat -> MyPosition -> Bool -> MyPieceInfo

$(genSingletons [''MyPieceInfo])

data MyPiece where
    MkPiece :: Team -> PieceName -> MyPieceInfo -> MyPiece

$(genSingletons [''MyPiece])
