{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString as BS(readFile)
import Data.Foldable as F
import Data.List.Split
import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import System.Environment

data Instruction = Add Int | Noop deriving (Show)

data State = State {
    register :: Int,
    elapsed :: Int -- Ticks since the current instruction started
} deriving (Show)

initial :: State
initial = State 1 0

-- Given a current state of the machine
-- (register value and how far in the pipe the current instruction is)
-- and a list of instructions to execute,
-- return the next state and remaining instructions.
tick :: State -> [Instruction] -> (State, [Instruction])
tick s [] = (s, [])
tick State{..} (i:next) = case i of
    Noop -> (State register 0, next) -- Nops take a single cycle.
    Add x -> if elapsed > 0 -- Adds take two.
        then (State (register + x) 0, next)
        else (State register (elapsed + 1), i:next)

-- A list of the register states at each cycle.
nthCycle :: [Instruction] -> [Int]
nthCycle instrs =  register . fst <$> boundedCycles where
    -- Stop when we're out of instructions.
    boundedCycles = takeWhile (not . F.null . snd) cycles
    -- Give us our (state, remaining instructions) list from tick
    cycles = iterate (uncurry tick) (initial, instrs)

parseInstructions :: Text -> [Instruction]
parseInstructions input = parseInstruction <$> T.lines input

parseInstruction :: Text -> Instruction
parseInstruction line
    | line == T.pack "noop" = Noop
    | head toks == T.pack "addx" = Add . read . T.unpack . last $ toks
    | otherwise = error $ "unknown instruction " <> T.unpack line
        where toks = T.words line

part1 :: [Instruction] -> Int
part1 input = sum $ sigStrength input <$> [20, 60, 100, 140, 180, 220]

sigStrength :: [Instruction] -> Int -> Int
sigStrength i n = n * (nthCycle i !! (n - 1))

data Coord = Coord {
    cx :: Int,
    cy :: Int
} deriving (Show, Eq, Ord)

part2 :: [Instruction] -> IO ()
part2 input = do
    let
        -- Index the register states by cycle count.
        indexedRegisters = zip (nthCycle input) [0..]
        -- The beam moves along the scanline and resets every 40 ticks
        pitch = 40
        beam t = t `mod` pitch
        -- The pixel is on if it's inside the sprite (+/- 1 register val)
        pixelOn (reg, t) = abs (beam t - reg) <= 1
        frame = pixelOn <$> indexedRegisters :: [Bool]
        -- Draw our frame.
        scanline s = do
            traverse_ (\x -> putChar $ if x then '#' else '.') s
            putChar '\n'
    traverse_ scanline $ chunksOf pitch frame

main :: IO ()
main = do
    args <- getArgs
    input <- parseInstructions . decodeUtf8 <$> BS.readFile (head args)
    print $ part1 input
    part2 input
