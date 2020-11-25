module Ranges where

import ChessTypes
import FirstClassFunctions
import Data.Type.Nat hiding (SNat(..))
import qualified GHC.TypeLits as TL

-- Generates a range between two TL.Nat values, non-inclusive of the first argument
data RangeBetween :: TL.Nat -> TL.Nat -> Exp [TL.Nat]
type instance Eval (RangeBetween n m) = Eval (RangeBetweenHelper n m (TL.CmpNat n m))

data RangeBetweenHelper :: TL.Nat -> TL.Nat -> Ordering -> Exp [TL.Nat]
type instance Eval (RangeBetweenHelper n m LT) = (n TL.+ 1) ': (Eval (RangeBetweenHelper (n TL.+ 1) m (TL.CmpNat (n TL.+ 1) m)))
type instance Eval (RangeBetweenHelper n m EQ) = '[]
type instance Eval (RangeBetweenHelper n m GT) = (n TL.- 1) ': (Eval (RangeBetweenHelper (n TL.- 1) m (TL.CmpNat (n TL.- 1) m)))

data RangeBetweenDTNat :: Nat -> Nat -> Exp [Nat]
type instance Eval (RangeBetweenDTNat n m) = Eval (Map TLNatToNat (Eval (RangeBetween (ToGHC n) (ToGHC m))))

-- -- Generates a range between two Char values, non-inclusive of the first argument
-- -- It will only go from lowercase "a" to lowercase "z"
-- data CharRangeBetween :: TL.Symbol -> TL.Symbol -> Exp [TL.Symbol]
-- type instance Eval (CharRangeBetween a b) = Eval (CharRangeBetweenHelper a b (CmpTL.Symbol a b))

-- data CharRangeBetweenHelper :: TL.Symbol -> TL.Symbol -> Ordering -> Exp [TL.Symbol]
-- type instance Eval (CharRangeBetweenHelper a b LT) = Eval (TakeWhile (CharLessThan b) (Eval (Map FromJust (Eval (Filter IsJust (Eval (Map ((Flip (:+)) a) (Eval (RangeBetweenDTNat 0 8)))))))))
-- type instance Eval (CharRangeBetweenHelper a b EQ) = '[]
-- type instance Eval (CharRangeBetweenHelper a b GT) = Eval (TakeWhile (CharGreaterThan b) (Eval (Map FromJust (Eval (Filter IsJust (Eval (Map ((Flip (:-)) a) (Eval (RangeBetweenDTNat 0 8)))))))))

-- data CharLessThan :: TL.Symbol -> TL.Symbol -> Exp Bool
-- type instance Eval (CharLessThan b a) = Eval (IsLTEQ (CmpTL.Symbol a b))

-- data CharGreaterThan :: TL.Symbol -> TL.Symbol -> Exp Bool
-- type instance Eval (CharGreaterThan b a) = Eval (IsGTEQ (CmpTL.Symbol a b))

-- Generates a range between two Column values, non-inclusive of the first argument
data ColRangeBetween :: Column -> Column -> Exp [Column]
type instance Eval (ColRangeBetween a b) = Eval (ColRangeBetweenHelper a b (CmpColumn a b))

data ColRangeBetweenHelper :: Column -> Column -> Ordering -> Exp [Column]
type instance Eval (ColRangeBetweenHelper a b LT) = Eval (TakeWhile (ColLessThan b) (Eval (Map FromJust (Eval (Filter IsJust (Eval (Map ((Flip (:+)) a) (Eval (RangeBetweenDTNat Nat0 Nat8)))))))))
type instance Eval (ColRangeBetweenHelper a b EQ) = '[]
type instance Eval (ColRangeBetweenHelper a b GT) = Eval (TakeWhile (ColGreaterThan b) (Eval (Map FromJust (Eval (Filter IsJust (Eval (Map ((Flip (:-)) a) (Eval (RangeBetweenDTNat Nat0 Nat8)))))))))

data ColLessThan :: Column -> Column -> Exp Bool
type instance Eval (ColLessThan b a) = Eval (IsLTEQ (CmpColumn a b))

data ColGreaterThan :: Column -> Column -> Exp Bool
type instance Eval (ColGreaterThan b a) = Eval (IsGTEQ (CmpColumn a b))

type family CmpColumn (x :: Column) (y :: Column) :: Ordering where
    CmpColumn A A = EQ
    CmpColumn A B = LT
    CmpColumn A C = LT
    CmpColumn A D = LT
    CmpColumn A E = LT
    CmpColumn A F = LT
    CmpColumn A G = LT
    CmpColumn A H = LT
    CmpColumn B A = GT
    CmpColumn C A = GT
    CmpColumn D A = GT
    CmpColumn E A = GT
    CmpColumn F A = GT
    CmpColumn G A = GT
    CmpColumn H A = GT
    CmpColumn x y = CmpColumn (ColDec x) (ColDec y)

-- Type family to decrement a column value!
type family ColDec (x :: Column) :: Column where
    ColDec A = A
    ColDec B = A
    ColDec C = B
    ColDec D = C
    ColDec E = D
    ColDec F = E
    ColDec G = F
    ColDec H = G

data IsLTEQ :: Ordering -> Exp Bool
type instance Eval (IsLTEQ LT) = True
type instance Eval (IsLTEQ EQ) = True
type instance Eval (IsLTEQ GT) = False

data IsGTEQ :: Ordering -> Exp Bool
type instance Eval (IsGTEQ LT) = False
type instance Eval (IsGTEQ EQ) = True
type instance Eval (IsGTEQ GT) = True