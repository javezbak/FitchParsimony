import Data.Bits
import Data.Maybe

-- Q = ?, all other letters are part of character set
data Alphabet = A | B | C |  D | E | F | G | H | Q deriving (Show, Eq, Bounded, Enum)
type FullAlphabet = Maybe Alphabet


-- example values: [Just B,Just B,Just C] [Just B,Just A,Just C]
-- returns: ([False,True,False],[True,True,False],[False,True,True])
dnaCompute :: [FullAlphabet] -> [FullAlphabet] -> ([[Bool]])
dnaCompute childNodeOne childNodeTwo =
    
    let 
        -- create boolean list of lists to test for presence of each alphabet type in child nodes
        nodeOnePresence = [dMatchList char childNodeOne | char <- allChars]
        nodeTwoPresence = [dMatchList char childNodeTwo | char <- allChars]

        -- put values together with AND operation
        rValues = zipWith (\x y -> dAnd x y) nodeOnePresence nodeTwoPresence

        -- xor all values into a single value
        tmp = dXorAllVals rValues

        aValues = [dOr rVal tmp | rVal <- rValues]

    in aValues
    where 
        allChars = [(minBound :: Alphabet) ..]


dMatchList :: Alphabet -> [FullAlphabet] -> [Bool]
dMatchList desiredChar charList = [dMatch desiredChar currChar | currChar <- charList]

dMatch :: Alphabet -> FullAlphabet -> Bool
dMatch desiredChar matchingChar
    | ((desiredChar == fromJust matchingChar) || (desiredChar == Q)) = True  
    | otherwise = False
  
dAnd :: Bits b => [b] -> [b] -> [b]
dAnd = zipBits (.&.)

dOr :: Bits b => [b] -> [b] -> [b]
dOr = zipBits (.|.)

dXorAllVals :: Bits b => [[b]] -> [b]
dXorAllVals [x] = x
dXorAllVals (x:xs) = dXor x (dXorAllVals xs)

dXor :: Bits b => [b] -> [b] -> [b]
dXor = zipBits xor

dFlip :: [Bool] -> [Bool]
dFlip = fmap complement

zipBits :: Bits b => (b -> b -> b) -> [b] -> [b] -> [b]
zipBits f = zipWith f



