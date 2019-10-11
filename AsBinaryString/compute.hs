import Data.Bits

-- A = 1, B = 0
data DNA = A | B deriving (Show, Eq)

-- original value(s): "1100111", "1010101", "0011111", "0101011"
-- new value(s): [A,A,B,B,A,A,A]
-- returns: [True,True,False,False,True,True,True]
-- example: verticalPacking A [A,A,B,B,A,A,A]
verticalPacking dnaChar aTen = 
     seekChar dnaChar aTen

seekChar :: DNA -> [DNA] -> [Bool]
seekChar dnaLetter dnaSequence =
    [if x == dnaLetter then True else False | x <- dnaSequence]

-- verticalPackingExpanded A [A,A,B,B,A,A,A] [A,B,A,B,A,B,A]
-- returns [[True, True], [True, False], [False, True], [False, False], [True, True], [True, False], [True, True]] 
verticalPackingExpanded dnaLetter aTen aEleven = 
    zipWith (\x y -> (if x == dnaLetter then True else False) : (if y == dnaLetter then True else False) : []) aTen aEleven