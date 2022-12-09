{-# LANGUAGE RecordWildCards #-}

import Control.Exception(assert)
import qualified Data.ByteString as BS(readFile)
import Data.Foldable
import Data.List as L
import Data.Set as S
import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import Data.Int
import System.Environment

data Instruction = Instruction {
    direction :: Char,
    steps :: Int
} deriving (Show)

parseInstructions :: Text -> [Instruction]
parseInstructions input = parseInstruction <$> T.lines input

parseInstruction :: Text -> Instruction
parseInstruction line = Instruction d s where
    toks = T.words line
    d = assert (T.length (head toks) == 1) T.head . head $ toks
    s = read . T.unpack . last $ toks

data Coord = Coord {
    cx :: Int32,
    cy :: Int32
} deriving (Show, Eq, Ord)

data Segment = Segment {
    segHead :: Coord,
    segTail :: Coord
} deriving (Show)

data TimeStep = TimeStep {
    rope :: [Coord],
    tailHistory :: Set Coord
} deriving (Show)

starting :: Int -> TimeStep
starting n = TimeStep (replicate n (Coord 0 0)) (S.singleton (Coord 0 0))

update :: Char -> TimeStep -> TimeStep
update c TimeStep{..} = TimeStep newRope newHistory
    where
        newRope = pullRope . moveHead c $ rope
        newHistory = tailHistory <> S.singleton (last newRope)

moveHead :: Char -> [Coord] -> [Coord]
moveHead d (x:xs) = move d x : xs
moveHead _ [] = error "empty rope"

pullRope :: [Coord] -> [Coord]
pullRope (x1:x2:xs) = x1:pullRope (moved:xs)
    where moved = moveTail (Segment x1 x2)
pullRope [x1] = [x1]
pullRope [] = error "empty rope"

move :: Char ->  Coord -> Coord
move dir (Coord x y)
    | dir == 'U' = Coord x (y + 1)
    | dir == 'D' = Coord x (y - 1)
    | dir == 'L' = Coord (x - 1) y
    | dir == 'R' = Coord (x + 1) y
    | otherwise = error $ "Unknown direction " ++ [dir]

moveTail :: Segment -> Coord
moveTail Segment{..} = newTail
    where
        dx = cx segHead - cx segTail
        dy = cy segHead - cy segTail
        manhattan = max (abs dx) (abs dy)
        newTail = if manhattan <= 1 then segTail else
            Coord (cx segTail + signum dx) (cy segTail + signum dy)


runInstruction :: TimeStep -> Instruction -> TimeStep
runInstruction ts Instruction{..} = iterate (update direction) ts !! steps

part1 :: [Instruction] -> Int
part1 instrs = length . tailHistory $ finalState
    where finalState = L.foldl' runInstruction (starting 2) instrs

part2 :: [Instruction] -> IO Int
part2 instrs = do
    let finalState = L.foldl' runInstruction (starting 10) instrs
    printTrail . tailHistory $ finalState
    pure . length . tailHistory $ finalState

printTrail :: Set Coord -> IO ()
printTrail hist = do
    let minX = S.foldl' (\acc c -> min acc (cx c)) 0 hist
        maxX = S.foldl' (\acc c -> max acc (cx c)) 0 hist
        minY = S.foldl' (\acc c -> min acc (cy c)) 0 hist
        maxY = S.foldl' (\acc c -> max acc (cy c)) 0 hist
        printLn y = do
            traverse_ (\x -> putChar $ if member (Coord x y) hist then '#' else '.') [minX..maxX]
            putChar '\n'
    traverse_ printLn $ enumFromThenTo maxY (maxY - 1) minY

main :: IO ()
main = do
    args <- getArgs
    input <- parseInstructions . decodeUtf8 <$> BS.readFile (head args)
    print $ part1 input
    part2 input >>= print
