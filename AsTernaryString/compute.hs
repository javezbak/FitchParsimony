import Data.Bits
import Data.Maybe

-- Q = ?, all others are part of character set
data Alphabet = A | B | C | Q deriving (Show, Eq)
type FullAlphabet = Maybe Alphabet

-- example values: [[Just B,Just B,Just C] [Just B,Just A,Just C]
-- returns: ([False,True,False],[True,True,False],[False,True,True])
dnaCompute :: [FullAlphabet] -> [FullAlphabet] -> ([Bool], [Bool], [Bool])
dnaCompute cNodeOne cNodeTwo =
    let 
        -- create boolean arrays to test for presence of each alphabet type in child nodes
        a1A :: [Bool]
        a1A = [dMatch A x  | x <- cNodeOne]

        a1B :: [Bool] 
        a1B = [dMatch B x | x <- cNodeOne]

        a1C :: [Bool]
        a1C = [dMatch C x | x <- cNodeOne]

        a2A :: [Bool]
        a2A = [dMatch A x  | x <- cNodeTwo]
         
        a2B :: [Bool]
        a2B = [dMatch B x | x <- cNodeTwo]

        a2C :: [Bool]
        a2C = [dMatch C x | x <- cNodeTwo]
            
        r0 = dAnd a1A a2A
        r1 = dAnd a1B a2B
        r2 = dAnd a1C a2C

        xorVal = dXor (dXor r0 r1) r2 --R0 XOR R1 XOR R2
        tmp = dFlip xorVal --NOT(R0 XOR R1 XOR R2)

    in (dOr tmp r0, dOr tmp r1, dOr tmp r2)

dMatch :: Alphabet -> FullAlphabet -> Bool
dMatch desiredChar matchingChar
    | ((desiredChar == fromJust matchingChar) || (desiredChar == Q)) = True  
    | otherwise = False
  
dAnd :: Bits b => [b] -> [b] -> [b]
dAnd = zipBits (.&.)

dOr :: Bits b => [b] -> [b] -> [b]
dOr = zipBits (.|.)

dXor :: Bits b => [b] -> [b] -> [b]
dXor = zipBits xor

dFlip :: [Bool] -> [Bool]
dFlip = fmap complement

zipBits :: Bits b => (b -> b -> b) -> [b] -> [b] -> [b]
zipBits f = zipWith f



