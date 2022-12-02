import qualified Data.ByteString as BS
import Data.Functor.Syntax
import Data.List
import Data.List.Split as DLS
import Data.Text as DT hiding(maximum, take)
import Data.Text.Read
import Data.Text.Encoding(decodeUtf8)
import Data.Word

generate :: Text -> [Word64]
generate input = ints
    where
        clumps = DLS.splitWhen (==empty) $ DT.lines input
        intClumps =  toInt <$$> clumps
        ints = sum <$> intClumps

toInt :: Text -> Word64
toInt t = case decimal t of
    Left _ -> error "not an int"
    Right (i, _) -> i

main :: IO ()
main = do
    input <- decodeUtf8 <$> BS.readFile "../input/2022/day1.txt"
    let vals = generate input
    print $ maximum vals
    print $ sum $ take 3 $ sortBy (flip compare) vals
