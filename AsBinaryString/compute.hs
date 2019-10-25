import Data.Bits

-- A = 1, B = 0
data DNA = A | B deriving (Show, Eq)

-- original value(s): "1100111"
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

-- original value(s): "1100111", "0011111", "1010101", "0101011"
-- new value(s): [A,A,B,B,A,A,A] [B,B,A,A,A,A,A] [A,B,A,B,A,B,A] [B,A,B,A,B,A,A]
dnaCompute :: [DNA] -> [DNA] ->  [DNA] -> [DNA] -> ([DNA], [DNA])
dnaCompute aTen aEleven aTwenty aTwentyOne =
    let rZero = dAnd aTen aTwenty --A10 AND A20
        rOne = dAnd aEleven aTwentyOne --A11 AND A21
        xorVal = dXor rZero rOne --R0 XOR R1
        tmp = dFlip xorVal --NOT(R0 XOR R1)
    in (dOr tmp rZero, dOr tmp rOne)


dAnd :: [DNA] -> [DNA] -> [DNA]
dAnd valOne valTwo =
    zipWith (\x y -> if x == A && y == A then A else B) valOne valTwo

dOr :: [DNA] -> [DNA] -> [DNA]
dOr valOne valTwo = 
    zipWith (\x y -> if x == A || y == A then A else B) valOne valTwo

dXor :: [DNA] -> [DNA] -> [DNA]
dXor valOne valTwo =
    zipWith (\x y -> if x == A && y == B || x == B && y == A then A else B) valOne valTwo

dFlip :: [DNA] -> [DNA]
dFlip val = 
    [if x == A then B else A | x <- val]



