import Control.Exception(assert)
import Data.ByteString as BS(ByteString, length, readFile, splitAt, unpack)
import qualified Data.ByteString.Char8 as C
import Data.List(foldl1')
import Data.List.Split
import Data.Word
import Data.Set

-- Priority (score) of a given item, [a-z,A-Z]
prioOf :: Word8 -> Word64
prioOf c = fromIntegral $ prioOf' $ fromIntegral c -- Just casts to int
    where
        prioOf' :: Int -> Int
        prioOf' i
            | i >= fromEnum 'a' && i <= fromEnum 'z' = i - fromEnum 'a' + 1
            | i >= fromEnum 'A' && i <= fromEnum 'Z' = i - fromEnum 'A' + 27
            | otherwise =  error "wat"


part1 :: ByteString -> Word64
part1 input = sum $ duplicatedItemScore <$> C.lines input

-- Convert a ByteString to a set of its bytes
toSet :: ByteString -> Set Word8
toSet = fromList . BS.unpack

-- Grab the element out of a single-elemnt set
onlyElement :: Set a -> a
onlyElement s = assert (size s == 1) $ head . toList $ s

-- Find the (score of the) duplicate item in both sides of the pack.
duplicatedItemScore :: ByteString -> Word64
duplicatedItemScore line = prioOf duped
    where
        half = BS.length line `div` 2 :: Int
        (left, right) = BS.splitAt half line
        (leftSet, rightSet) = (toSet left, toSet right)
        duped = onlyElement $ intersection leftSet rightSet

part2 :: ByteString -> Word64
part2 input = sum $ groupScore <$> chunksOf 3 (C.lines input)

-- Find the (score of the) common item in all three packs.
groupScore :: [ByteString] -> Word64
groupScore group = prioOf . onlyElement $ shared
    where
        sets = toSet <$> group
        shared = foldl1' intersection sets

main :: IO ()
main = do
    input <- BS.readFile "../input/2022/day3.txt"
    print $ part1 input
    print $ part2 input
