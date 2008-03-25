{- | 
 Thomas Parslow (tom@almostobsolete.net)

 Implementation of a Bloom filter with a pure interface

 There's some useful information about bloom filters here:
<http://www.cs.wisc.edu/~cao/papers/summary-cache/node8.html> -}

module Data.Bloom(Bloom, bloom, add, test) where

import Data.Word (Word32, Word8)
import Data.Array.Diff (DiffUArray)
import Data.Bits(Bits, (.&.),(.|.), setBit, testBit)
import Data.Array.IArray(listArray, (//), (!))
import List

data Bloom keytype = MkBloom {hashfunc :: (keytype -> [Word8]),
                              array :: (DiffUArray Int Word32),
                              hashcount :: Int,
                              size :: Integer}

wordsize :: Integer
wordsize = 32

ispow2 :: Bits a => a -> Bool
ispow2 x = (x/=0) && ((x .&. (x-1)) == 0)

splitup :: Int -> [a] -> [[a]]
splitup _ [] = []
splitup n lst = let (x, rest) = splitAt n lst in
               x : splitup n rest

indexes :: Bloom a -> a -> [(Int,Int)]
indexes b key = [(fromIntegral $ x `div` wordsize,
                  fromIntegral $ x .&. (wordsize-1)) | x <- bitindexes]
   where bitindexes = take (hashcount b) $ map (.&. ((size b)-1)) ints
         ints = map bytes2int bytegroups
         bytegroups = splitup bytecount $ (hashfunc b) key
         bytes2int = foldr ((. (256 *)) . (+)) 0 . (map toInteger)
         bytecount = ceiling $ (logBase 2 (fromIntegral $ size b :: Double)/8)

-- |Creates a new Bloom Filter given a hash function, a size in bits
-- and the number of hashes to use per entry.
-- 
-- The hash function should take an item and return a infinite stream
-- of 'Data.Word.Word8', this will be evalutated as needed and cut up into the
-- required number of hashes.
--
-- An example hash function for strings using 'Data.Digest.MD5':
--
-- > md5hash s = Data.Digest.MD5.hash (map (fromIntegral.ord) s) ++ md5hash ('a':s)
bloom :: (a -> [Word8]) -> Integer -> Int -> Bloom a
bloom hf sz hc = if ispow2 sz
                 then b
                 else error "Bloom.bloom: Size must be a power of 2"
    where b  = MkBloom hf ary hc sz
          ary = (listArray (0, wordc-1) (repeat 0))
          wordc = ceiling $ (fromIntegral sz)/(fromIntegral wordsize :: Double)

-- |Adds an item to the Bloom Filter and returns the modified version
add :: Bloom a -> a -> Bloom a
add b key = b {array = ary}
   where ary = (array b) // newvalues
         newvalues = foldr combine [] $ sort [(i, setBit ((array b) ! i) bit)
                                                  | (i,bit) <- indexes b key]
         combine x acc@(a:as) 
             | fst x == fst a = (fst x,(snd x .|. snd a)):as
             | otherwise = x:acc
         combine x [] = [x]

-- |Returns True if the given item may have been added to this filter
-- before and False if it definitely hasn't.
test :: Bloom a -> a -> Bool
test b key = all p $ indexes b key
   where p (aidx, bidx) = testBit ((array b) ! aidx) bidx