{-# LANGUAGE DeriveTraversable #-}

import Control.Lens
import Control.Exception(assert)
import qualified Data.ByteString as BS(readFile)
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Semigroup
import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import Data.Int

type Directory a = M.Map Text (Node a)

data Node a = Dir (Directory a) | File a
    deriving (Show, Eq, Functor, Foldable, Traversable)

type SizedDir = Directory Int64
type SizedNode = Node Int64

parseDirs :: Text -> SizedNode
parseDirs = Dir . root . T.lines

root :: [Text] -> SizedDir
root (rl:rest) = assert (rl == T.pack "$ cd /") check
    where
        (parsed, remains) = appendToDir M.empty rest
        check = assert (null remains) parsed
root _ = error "dir with no commands"

appendToDir :: SizedDir -> [Text] -> (SizedDir, [Text])
appendToDir d [] = (d, [])
appendToDir d (x:xs)
    | x == T.pack "$ cd .." = (d, xs)
    | x == T.pack "$ ls" = uncurry appendToDir $ appendLs d xs
    | otherwise = assert (T.isPrefixOf (T.pack "$ cd ") x) $
        appendToDir added remains where
            added = M.insert name (Dir child) d
            -- Append to the existing entry.
            -- (appendLs should have created it prior to cd-ing in,
            --  but it should be mpty.)
            (child, remains) = assert (d M.! name == Dir M.empty) $ appendToDir M.empty xs
            name = T.splitAt 5 x ^. _2

appendLs :: SizedDir -> [Text] -> (SizedDir, [Text])
appendLs d [] = (d, [])
appendLs d (x:xs)
    | T.isPrefixOf (T.pack "$ ") x = (d, x:xs) -- Bail once we see the next command
    | T.isPrefixOf (T.pack "dir ") x = appendLs (M.insert (T.splitAt 4 x ^._2) (Dir M.empty) d) xs
    | otherwise = appendLs (M.insert name (File s) d) xs
        where
            toks = T.words x
            name = toks !! 1
            s = read . T.unpack . head $ toks

asDir :: SizedNode -> SizedDir
asDir (Dir d) = d
asDir (File _ ) = error "is a file"

foldDirs :: Monoid m => (SizedDir -> m) ->  SizedDir -> m
foldDirs f d = f d <> foldMap' id (justDirs <$> d)
    where
        justDirs (Dir d') = foldDirs f d'
        justDirs (File _) = mempty

part1 :: SizedNode -> Int64
part1 d = getSum $ foldDirs (Sum . under100k) (asDir d)
    where
        under100k :: SizedDir -> Int64
        under100k c = let s = sum (Dir c) in
            if s <= 100000 then s else 0

part2 :: SizedNode -> Int64
part2 input = getMin $ foldDirs (Min . atLeastNeeded) (asDir input)
    where
        available = 70000000 - sum input
        needed = 30000000 - available;
        atLeastNeeded :: SizedDir -> Int64
        atLeastNeeded d = let s = sum (Dir d) in
            if s >= needed then s else maxBound

main :: IO ()
main = do
    input <- parseDirs . decodeUtf8 <$> BS.readFile "../input/2022/day7.txt"
    print $ part1 input
    print $ part2 input
