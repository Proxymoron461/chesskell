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

-- Transform various types into singleton types (for use at the value-level)
$(genSingletons [''PieceName])
$(genSingletons [''Team])
$(genSingletons [''Nat])
$(genSingletons [''Column])
$(genSingletons [''Position])
$(genSingletons [''PieceInfo])
$(genSingletons [''Piece])
