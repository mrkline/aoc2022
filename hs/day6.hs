import qualified Data.ByteString as BS(readFile)
import Data.List
import qualified Data.Set as S
import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)

-- Bend space and time
-- https://stackoverflow.com/a/27733778/713961
windows :: Int -> [a] -> [[a]]
windows w = foldr (zipWith (:)) (repeat []) . take w . tails

uniqueWindowIndex :: Int -> Text -> Int
uniqueWindowIndex w input = windowIndex
    where
        windowIndex = case find unique indexed of
            Just (i, _) -> i + w
            Nothing -> error "no unique window"
        unique (_, wnd) = length (S.fromList wnd) == length wnd
        indexed = zip [0..] . windows w $ T.unpack input

part1 :: Text -> Int
part1 = uniqueWindowIndex 4

part2 :: Text -> Int
part2 = uniqueWindowIndex 14


main :: IO ()
main = do
    input <- decodeUtf8 <$> BS.readFile "../input/2022/day6.txt"
    print $ part1 input
    print $ part2 input
