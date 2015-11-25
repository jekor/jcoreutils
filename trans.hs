import Control.Monad (when)
import Data.ByteString (ByteString, length, hGetSome, hPut, concat)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Search (replace)
import System.Posix.Env.ByteString (getArgs)
import System.IO (hIsEOF, hFlush, stdin, stdout)

import Prelude hiding (length, concat)

main :: IO ()
main = do
  replacements <- uncurry zip . every2nd <$> getArgs
  trans' replacements
 where trans' rs = do
         s <- hGetSome stdin 8096
         when (length s > 0) ((hPut stdout $ trans rs s) >> hFlush stdout)
         eof <- hIsEOF stdin
         if eof
           then return ()
           else trans' rs

trans :: [(ByteString, ByteString)] -> ByteString -> ByteString
trans [] input = input
trans ((a, b):x) input = trans x $ concat $ BL.toChunks $ replace a b input

every2nd :: [a] -> ([a], [a])
every2nd = foldr (\a ~(x,y) -> (a:y,x)) ([],[])

