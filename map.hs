import qualified Data.ByteString as B
import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (isEOF, hClose)
import System.Process (createProcess, shell, CreateProcess(std_in), StdStream(CreatePipe), waitForProcess)

main :: IO ()
main = do
  args <- getArgs
  mapLines $ intercalate " " args

mapLines :: String -> IO ()
mapLines cmd = do
  eof <- isEOF
  if eof
     then return ()
     else do
       line <- B.getLine
       (Just h, _, _, p) <- createProcess $ (shell cmd) {std_in = CreatePipe}
       B.hPutStrLn h line
       hClose h
       _ <- waitForProcess p
       -- Technically, we'd want to alert the user to a non-zero exit code, but
       -- not all programs are well-behaved.
       mapLines cmd