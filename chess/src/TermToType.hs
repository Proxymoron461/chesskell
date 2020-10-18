module TermToType where

import Data.Proxy(Proxy(..))
import ChessTypes
import GHC.TypeLits

-- Constraints for transforming term-level values to type-level, erm, types

-- Transform term-level piece name into type-level one
class KnownPieceName (k :: PieceName) where
    mkTypePiece :: PieceName -> Proxy k

instance KnownPieceName 'Pawn where
    mkTypePiece Pawn = Proxy @Pawn

instance KnownPieceName 'Rook where
    mkTypePiece Rook = Proxy @Rook

instance KnownPieceName 'Bishop where
    mkTypePiece Bishop = Proxy @Bishop

instance KnownPieceName 'Knight where
    mkTypePiece Knight = Proxy @Knight

instance KnownPieceName 'King where
    mkTypePiece King = Proxy @King

instance KnownPieceName 'Queen where
    mkTypePiece Queen = Proxy @Queen
