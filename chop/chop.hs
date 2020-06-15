import Data.List (union)
import qualified Data.Text as T
import Data.Text.IO (getLine, putStrLn)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, isEOF)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Combinator (choice, sepBy1, many1, eof, option)
import Text.Parsec.Prim (parse, try)
import Text.Parsec.String (Parser)

import Prelude hiding (getLine, putStrLn)

type Range = (Int, Int)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [delimiter, rangeString] -> case parse (sepBy1 range (char ',') <* eof) "" rangeString of
                                  Left err     -> hPutStrLn stderr $ "invalid range: " ++ show err
                                  Right ranges -> chopLines (T.pack delimiter) ranges
    _ -> hPutStrLn stderr "Usage: chop delimiter range(s)"

chopLines :: T.Text -> [Range] -> IO ()
chopLines delimiter ranges = do
  eof' <- isEOF
  if eof'
     then return ()
     else do
       parts <- filter (/= "") . T.splitOn delimiter <$> getLine
       putStrLn $ T.intercalate delimiter $ takeRanges ranges parts
       chopLines delimiter ranges

takeRanges :: [Range] -> [T.Text] -> [T.Text]
takeRanges ranges parts =
  let mapped = foldr1 union $ map (\r -> mapRange r (length parts)) ranges in
  map (\pos -> parts !! (pos - 1)) mapped

-- Given a range, map it onto a list of the given size.
mapRange :: Range -> Int -> [Int]
mapRange (begin', end') size =
  let (begin, end) = (pos begin', pos end') in
  if end < begin || begin > size || end < 1
     then []
     else [max 1 begin .. min size end]
 where pos i = if i < 0 then size + i + 1 else i

range :: Parser Range
range = choice [try continuousRange, singleNum]

number :: Parser Int
number = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

-- "1" or "-2"
singleNum :: Parser Range
singleNum = (\x -> (x,x)) <$> number

-- "1..2" or "1..-1"
continuousRange :: Parser Range
continuousRange = (,) <$> number <* string ".." <*> number
