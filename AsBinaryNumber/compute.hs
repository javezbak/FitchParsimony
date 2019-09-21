import Data.Bits

getNumBits :: Int -> Int --gets the number of bits in the binary representation of x
getNumBits x
    | x == 0 = 0
    | otherwise = 1 + (getNumBits y)
    where y = (shiftR x 1)


-- compute 103 31 85 43 (example values from the paper)
compute aTen aEleven aTwenty aTwentyOne = 
    let rZero = (.&.) aTen aTwenty; --A10 AND A20
        rOne = (.&.) aEleven aTwentyOne --A11 AND A21
    in  
    let xorVal = (xor rZero rOne); --R0 XOR R1
        allOnes =  (2^(getNumBits (xorVal)))-1
    in 
    let tmp = xor xorVal allOnes --tmp = NOT(R0 XOR R1)
    in ((.|.) rZero tmp, (.|.) rOne tmp) 