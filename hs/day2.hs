import qualified Data.ByteString as BS
import qualified Data.Text as DT
import Data.Text.Encoding(decodeUtf8)
import Data.Word

data Move = Rock | Paper | Scissors deriving(Show, Eq)

parseMove :: Char -> Move
parseMove 'A' = Rock
parseMove 'B' = Paper
parseMove 'C' = Scissors
parseMove 'X' = Rock
parseMove 'Y' = Paper
parseMove 'Z' = Scissors
parseMove _ = error "bad input"

valueOf :: Move -> Word64
valueOf Rock = 1
valueOf Paper = 2
valueOf Scissors = 3

findWinner :: Move -> Move
findWinner Rock = Paper
findWinner Paper = Scissors
findWinner Scissors = Rock

findLoser :: Move -> Move
findLoser Rock = Scissors
findLoser Paper = Rock
findLoser Scissors = Paper

part1 :: DT.Text -> Word64
part1 input = sum scores
    where
        lineScore line = score (parseMove $ DT.head line) (parseMove $ DT.last line)
        scores = lineScore <$> DT.lines input

score :: Move -> Move -> Word64
score them us = valueOf us +
    if us == them then 3
    else if us == findWinner them then 6
    else 0

part2 :: DT.Text -> Word64
part2 input = sum scores
    where
        theirs l = parseMove $ DT.head l
        lineScore l = case DT.last l of
            'X' -> valueOf . findLoser . theirs $ l
            'Y' -> (valueOf . theirs $ l) + 3
            'Z' -> (valueOf . findWinner . theirs $ l) + 6
            _ -> error "bad input"
        scores = lineScore <$> DT.lines input

main :: IO ()
main = do
    input <- decodeUtf8 <$> BS.readFile "../input/2022/day2.txt"
    print $ part1 input
    print $ part2 input
