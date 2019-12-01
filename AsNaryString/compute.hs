import Data.Bits

-- example values: "BBC" "BAC" "ABCDEFGHQ"
-- returns: ([False,True,False],[True,True,False],[False,True,True])
dnaCompute :: String -> String -> String -> ([[Bool]])
dnaCompute childNodeOne childNodeTwo characterSet =
    
    let 
        -- create boolean list of lists to test for presence of each alphabet type in child nodes
        nodeOnePresence = [dMatchList chr childNodeOne | chr <- characterSet]
        nodeTwoPresence = [dMatchList chr childNodeTwo | chr <- characterSet]

        -- put values together with AND operation
        rValues = zipWith (\x y -> dAnd x y) nodeOnePresence nodeTwoPresence

        -- xor all values into a single value
        tmp = dXorAllVals rValues

        aValues = [dOr rVal tmp | rVal <- rValues]

    in aValues


dMatchList :: Char -> String -> [Bool]
dMatchList desiredChr chrList = [dMatch desiredChr currChr | currChr <- chrList]

dMatch :: Char -> Char -> Bool
dMatch desiredChr matchingChr
    | ((desiredChr == matchingChr) || (desiredChr == 'Q')) = True  
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
