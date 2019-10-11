import Data.Bits

data DNA = A | B deriving (Show, Eq)

-- A = 1, B = 0
-- original values: "1100111", "1010101", "0011111", "0101011"
-- new values: [A,A,B,B,A,A,A], [A,B,A,B,A,B,A]
-- returns: 
-- example: verticalPacking [A,A,B,B,A,A,A] [A,B,A,B,A,B,A]
verticalPacking aTen aEleven = 
     seekA A aTen

seekA :: DNA -> [DNA] -> [Bool]
seekA dnaLetter dnaSequence =
    [if x == dnaLetter then True else False | x <- dnaSequence]