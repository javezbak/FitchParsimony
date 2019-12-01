import Data.Bits

zipBits :: Bits b => (b -> b -> b) -> [b] -> [b] -> [b]
zipBits f = zipWith f


dXor :: Bits b => [b] -> [b] -> [b]
dXor = zipBits xor

dXorAllVals :: Bits b => [b] -> [b] -> [b]
dXorAllVals [] = 0
dXorAllVals (x:xs) = dXor x (dXor dXorAllVals xs)