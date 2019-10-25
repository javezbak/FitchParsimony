-- A = 1, B = 0, Q = ?
data DNA = A | B | Q deriving (Show, Eq)

-- original value(s): [B,B,A,A,Q,Q,Q] [B,A,B,A,B,A,Q]
dnaCompute :: [DNA] -> [DNA] -> ([DNA], [DNA])
dnaCompute cNodeOne cNodeTwo =
    let aTen = [if x == B || x == Q then A else B | x <- cNodeOne]
        aEleven = [if x == A || x == Q then A else B | x <- cNodeOne]
        aTwenty = [if x == B || x == Q then A else B | x <- cNodeTwo]
        aTwentyOne = [if x == A || x == Q then A else B | x <- cNodeTwo]

        rZero = dAnd aTen aTwenty --A10 AND A20
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



