import Data.Bits

-- A = 1, B = 0, Q = ?
data Alphabet = A | B | Q deriving (Show, Eq)
type FullAlphabet = Maybe Alphabet

-- original value(s): [B,B,A,A,Q,Q,Q] [B,A,B,A,B,A,Q]
-- returns: ([True,True,True,False,True,False,True],[False,True,True,True,False,True,True])
dnaCompute :: [FullAlphabet] -> [FullAlphabet] -> ([Bool], [Bool])
dnaCompute cNodeOne cNodeTwo =
    let a10 :: [Bool]
        a10 = [notA x  | x <- cNodeOne]

        a20 :: [Bool]
        a20 = [notA x  | x <- cNodeTwo]
         
        a11 :: [Bool] 
        a11 = [notB x | x <- cNodeOne]

        a21 :: [Bool]
        a21 = [notB x | x <- cNodeTwo]
            
        r0 = dAnd a10 a20 --A10 AND A20
        r1 = dAnd a11 a21 --A11 AND A21

        xorVal = dXor r0 r1 --R0 XOR R1
        tmp = dFlip xorVal --NOT(R0 XOR R1)

    in (dOr tmp r0, dOr tmp r1)
    where notA :: FullAlphabet -> Bool
          notA (Just A) = False
          notA _ = True

          notB :: FullAlphabet -> Bool
          notB (Just B) = False
          notB _ = True


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



