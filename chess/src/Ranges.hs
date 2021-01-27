module Ranges where

import ChessTypes
import FirstClassFunctions
import FingerTree
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

type family BetweenColumns (x :: Column) (y :: Column) :: FingerTree Column where
    BetweenColumns x x = Empty
    BetweenColumns A B = Single B
    BetweenColumns A C = Deep (One B) Empty (One C)
    BetweenColumns A D = Deep (Two B C) Empty (One D)
    BetweenColumns A E = Deep (Two B C) Empty (Two D E)
    BetweenColumns A F = Deep (Three B C D) Empty (Two E F)
    BetweenColumns A G = Deep (Three B C D) Empty (Three E F G)
    BetweenColumns A H = Deep (Four B C D E) Empty (Three F G H)
    BetweenColumns B C = Single C
    BetweenColumns B D = Deep (One C) Empty (One D)
    BetweenColumns B E = Deep (Two C D) Empty (One E)
    BetweenColumns B F = Deep (Two C D) Empty (Two E F)
    BetweenColumns B G = Deep (Two C D) Empty (Three E F G)
    BetweenColumns B H = Deep (Three C D E) Empty (Three F G H)
    BetweenColumns C D = Single D
    BetweenColumns C E = Deep (One D) Empty (One E)
    BetweenColumns C F = Deep (Two D E) Empty (One F)
    BetweenColumns C G = Deep (Two D E) Empty (Two F G)
    BetweenColumns C H = Deep (Three D E F) Empty (Two G H)
    BetweenColumns D E = Single E
    BetweenColumns D F = Deep (One E) Empty (One F)
    BetweenColumns D G = Deep (One E) Empty (Two F G)
    BetweenColumns D H = Deep (Two E F) Empty (Two G H)
    BetweenColumns E F = Single F
    BetweenColumns E G = Deep (One F) Empty (One G)
    BetweenColumns E H = Deep (One F) Empty (Two G H)
    BetweenColumns F G = Single G
    BetweenColumns F H = Deep (One G) Empty (One H)
    BetweenColumns G H = Single H
    BetweenColumns H G = Single G
    BetweenColumns H F = Deep (One G) Empty (One F)
    BetweenColumns H E = Deep (Two G F) Empty (One E)
    BetweenColumns H D = Deep (Two G F) Empty (Two E D)
    BetweenColumns H C = Deep (Three G F E) Empty (Two D C)
    BetweenColumns H B = Deep (Three G F E) Empty (Three D C B)
    BetweenColumns H A = Deep (Four G F E D) Empty (Three C B A)
    BetweenColumns G F = Single F
    BetweenColumns G E = Deep (One F) Empty (One E)
    BetweenColumns G D = Deep (Two F E) Empty (One D)
    BetweenColumns G C = Deep (Two F E) Empty (Two D C)
    BetweenColumns G B = Deep (Two F E) Empty (Three D C B)
    BetweenColumns G A = Deep (Three F E D) Empty (Three C B A)
    BetweenColumns F E = Single E
    BetweenColumns F D = Deep (One E) Empty (One D)
    BetweenColumns F C = Deep (Two E D) Empty (One C)
    BetweenColumns F B = Deep (Two E D) Empty (Two C B)
    BetweenColumns F A = Deep (Three E D C) Empty (Two B A)
    BetweenColumns E D = Single D
    BetweenColumns E C = Deep (One D) Empty (One C)
    BetweenColumns E B = Deep (One D) Empty (Two C B)
    BetweenColumns E A = Deep (Two D C) Empty (Two B A)
    BetweenColumns D C = Single C
    BetweenColumns D B = Deep (One C) Empty (One B)
    BetweenColumns D A = Deep (One C) Empty (Two B A)
    BetweenColumns C B = Single B
    BetweenColumns C A = Deep (One B) Empty (One A)
    BetweenColumns B A = Single A