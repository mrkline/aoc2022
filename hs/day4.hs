import Control.Exception(assert)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS(readFile)
import qualified Data.ByteString.Char8 as C
import Data.Word

data Range = Range {
    start, end :: Word8
}

rangeLength :: Range -> Word8
rangeLength a = end a - start a

rangeContains :: Range -> Range -> Bool
rangeContains a b = start big <= start small && end big >= end small
    where
        (big, small) = if rangeLength a >= rangeLength b then (a, b) else (b, a)

rangeOverlaps :: Range -> Range -> Bool
rangeOverlaps a b = start a  <= end b && end a >= start b

toPairs :: ByteString -> (Range, Range)
toPairs line = assert (length parsePair == 2) (elf1, elf2)
    where
        parsePair = parseRange <$> C.split ',' line
        parseElems r = read . C.unpack <$> C.split '-' r :: [Word8]
        assembleRange rl = assert (length rl == 2) $ Range (head rl) (last rl)
        parseRange = assembleRange . parseElems
        elf1 = head parsePair
        elf2 = last parsePair

part1 :: [(Range, Range)] -> Int
part1 = length . filter (uncurry rangeContains)

part2 :: [(Range, Range)] -> Int
part2 = length . filter (uncurry rangeOverlaps)

main :: IO ()
main = do
    input <- BS.readFile "../input/2022/day4.txt"
    let pairs = toPairs <$> C.lines input
    print $ part1 pairs
    print $ part2 pairs
