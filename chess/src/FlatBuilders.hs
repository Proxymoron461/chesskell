module FlatBuilders where

-- A continuation has form (t -> m)
type Spec t = forall m. (t -> m) -> m

type Conv s t = s -> Spec t

type Term t r = t -> r

-- Flat builder example 1 from Mezzo
string :: String -> Spec String
string arg cont = cont arg

firstChar :: Conv String Char
firstChar str cont = cont (head str)

printAscii :: Term Char Int
printAscii = fromEnum

put :: Show a => Term a String
put = show

-- string "Hello" firstChar printAscii = 72
-- string "Hello" firstChar printChar = "'H'"

-- Flat builder example 2 from Mezzo
-- add :: Int -> ((Int -> Int) -> m) -> m
-- add 5 :: ((Int -> Int) -> m) -> m
-- add 5 _, _ :: (Int -> Int) -> m
add :: Int -> Spec (Int -> Int)
add arg cont = cont (arg +)

to :: (Int -> Int) -> Conv Int Int
to f x cont = cont (f x)

and' :: Conv a a
and' x cont = cont x

the :: Conv a a
the = and'

display :: Show a => Conv a String
display s cont = cont (show s)

result :: Term String String
result s = "Result: " ++ s

-- add 5 to 7 and' display the result = "Result: 12"
