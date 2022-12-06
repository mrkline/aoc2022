import Control.Lens((&), (%~), element)
import qualified Data.ByteString as BS(readFile)
import Data.List
import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import Data.Vector((!), Vector, fromList)
import Text.Regex.TDFA((=~))

data Step = Step {
    count :: Int,
    from :: Int,
    to :: Int
} deriving Show

data Input = Input {
    stacks :: Vector [Char],
    directions:: Vector Step
} deriving Show

parseInput :: Text -> Input
parseInput i = Input s d
    where
        halves = T.splitOn (T.pack "\n\n") i
        s = parseStacks . head $ halves
        d = parseDirections . last $ halves

parseStacks :: Text -> Vector [Char]
parseStacks st = fromList onStacks
    where
        -- Grab the last line.
        (lastLine:revLines) = reverse . T.lines $ st
        -- Split that last line; create an empty stack for each token ("word").
        emptyStacks = replicate (length . T.words $ lastLine) [] :: [[Char]]
        -- Iterate through every other line, appending it to the stacks.
        onStacks = foldl' addLine emptyStacks revLines
        -- For stack s at index i, append iff there's a crate there.
        addLine prev l = (\(s, i) -> pushCrate s $ crateChar l i) <$> zip prev [0..]
        pushCrate stack c = if c == ' ' then stack else c:stack
        -- Crates are formatted
        -- [a] [b] [c] [d]
        -- so char i*4 + 1 is the contents of the crate.
        crateChar :: Text -> Int -> Char
        crateChar l i = T.index l (i * 4 + 1)

parseDirections :: Text -> Vector Step
parseDirections d = fromList $ parseDirection <$> T.lines d
    where
        parseDirection = dirFromList . nums . T.unpack
        -- We'd better have three matches in the regex below.
        dirFromList [c, f, t] = Step c (f - 1) (t - 1)
        dirFromList _ = error "bad move"
        -- List of substring captures to list of ints
        nums l = read <$> substrs l :: [Int]
        -- Regex capture the numbers out of "move A from B to C".
        substrs l = s where (_, _, _, s) = reg l
        reg :: String -> (String, String, String, [String])
        reg l = l =~ "move ([0-9]+) from ([0-9]+) to ([0-9]+)"

part1 :: Input -> String
part1 i = foldl' (\acc s -> acc ++ [head s]) [] restacked
    where
        restacked = foldl' applyStep (stacks i) (directions i)
        -- Apply moveOneCrate `count stp` times
        applyStep :: Vector [Char] -> Step -> Vector [Char]
        applyStep onStacks stp = iterate (moveOneCrate stp) onStacks !! count stp
        -- Pop the top off of the `from stp`-th stack and push it to the `to stp`-th
        -- Lenses ftw? https://stackoverflow.com/a/15531874/713961
        moveOneCrate :: Step -> Vector [Char] -> Vector [Char]
        moveOneCrate stp stk = (stk & element (from stp) %~ tail) -- pop
            & element (to stp) %~ (head (stk ! from stp):) -- push


part2 :: Input -> String
part2 i = foldl' (\acc s -> acc ++ [head s]) [] restacked
    where
        restacked = foldl' applyStep (stacks i) (directions i)
        -- Pop n crates off the top of one stack and push them on the other.
        applyStep :: Vector [Char] -> Step -> Vector [Char]
        applyStep stk stp = (stk & element (from stp) %~ drop (count stp)) -- pop
            & element (to stp) %~ (movingCrates stk stp ++ ) -- push
        movingCrates :: Vector [Char] -> Step -> [Char]
        movingCrates stk stp = take (count stp) (stk ! from stp)

main :: IO ()
main = do
    input <- parseInput . decodeUtf8 <$> BS.readFile "../input/2022/day5.txt"
    print $ part1 input
    print $ part2 input
