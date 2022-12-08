{-# LANGUAGE DeriveTraversable #-}

import Control.Lens
import Control.Exception(assert)
import qualified Data.ByteString as BS(readFile)
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import Data.Int

type Directory a = M.Map Text (Node a)

data Node a = Dir (Directory a) | File a
    deriving (Show, Eq)

instance Functor Node where
    fmap f (File a) = File $ f a
    fmap f (Dir d) = Dir $ (fmap . fmap) f d

type SizedDir = Directory Int64

parseDirs :: Text -> SizedDir
parseDirs input = root . T.lines $ input

root :: [Text] -> SizedDir
root (rl:rest) = assert (rl == T.pack "$ cd /") check
    where
        (parsed, remains) = appendToDir M.empty rest
        check = assert (remains == []) parsed
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
            name = (T.splitAt 5 x)^._2

appendLs :: SizedDir -> [Text] -> (SizedDir, [Text])
appendLs d [] = (d, [])
appendLs d (x:xs)
    | T.isPrefixOf (T.pack "$ ") x = (d, (x:xs)) -- Bail once we see the next command
    | T.isPrefixOf (T.pack "dir ") x = appendLs (M.insert ((T.splitAt 4 x)^._2) (Dir M.empty) d) xs
    | otherwise = appendLs (M.insert name (File s) d) xs
        where
            toks = T.words x
            name = toks !! 1
            s = read . T.unpack $ toks !! 0

main :: IO ()
main = do
    input <- parseDirs . decodeUtf8 <$> BS.readFile "/tmp/test.txt"
    print $ (const 42) <$> input
