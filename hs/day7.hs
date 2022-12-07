{-# LANGUAGE DeriveTraversable #-}

import Control.Exception(assert)
import qualified Data.ByteString as BS(readFile)
import qualified Data.Map.Strict as M
import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import Data.Int

type Directory a = M.Map String (Node a)

data Node a = Dir (Directory a) | File a
    deriving (Show, Foldable, Functor, Traversable)

type SizedDir = Directory Int64

parseDirs :: Text -> SizedDir
parseDirs input = root . T.lines $ input

root :: [Text] -> SizedDir
root (rl:rest) = assert (rl == T.pack "$ cd /") assemble
    where
        assemble = parseDir rest
root _ = error "dir with no commands"

parseDir :: [Text] -> SizedDir
parseDir (x:xs) = assert (T.isPrefixOf (T.pack "$ ") x) go
    where
        go = M.empty

part1 :: SizedDir -> Int64
part1 _ = 42

part2 :: SizedDir -> Int64
part2 _ = 42


main :: IO ()
main = do
    input <- parseDirs . decodeUtf8 <$> BS.readFile "../input/2022/day7.txt"
    print input
    print $ part1 input
    print $ part2 input
