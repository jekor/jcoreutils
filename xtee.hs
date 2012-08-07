-- Copyright 2008, 2009, 2010, 2012 Chris Forno
-- This program is distributed under the terms of
-- the GNU General Public License (version 3).

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

import System.IO
import System.Console.GetOpt
import System.Environment
import System.Exit
import Control.Concurrent
import Control.Monad

version  :: Double
version  =  1.0

usage    :: String
usage    =  "Usage: xtee [option...] -i input-file -o output-file"

data Flag = Help | Version | InFile String | OutFile String | DaemonMode
            deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
 Option ['h'] ["help"]    (NoArg Help)             "show help",
 Option ['v'] ["version"] (NoArg Version)          "show version",
 Option ['i'] ["input"]   (ReqArg InFile   "FILE") "input file",
 Option ['o'] ["output"]  (ReqArg OutFile  "FILE") "output file",
 Option ['d'] ["daemon"]  (NoArg DaemonMode)       "daemon mode" ]

xteeOptions :: [String] -> IO ([Flag])
xteeOptions argv =
  case getOpt Permute options argv of
    ([], _,[])   -> ioError (userError ("\n" ++ showUsage))
    (o,  _,[])   -> if Help `elem` o
                      then putStr showUsage >> exitWith ExitSuccess
                      else if Version `elem` o
                             then putStr (showVersion ++ "\n") >> exitWith ExitSuccess
                             else return o
    (_,  _,errs) -> ioError (userError (concat errs ++ showUsage))
  where  showVersion   =  "xtee version " ++ show version
         usageHeader   =  unlines [showVersion, usage]
         showUsage     =  usageInfo usageHeader options

-- xtee copies its stdin to a file and copies another file to its stdout. We
-- first spawn a process (a Haskell process, not an OS process) to pass stdin
-- through to the output file and then we pass the input file though to stdout.
-- To make sure that both complete before we exit, we use the wait MVar.

xtee :: IO Handle -> IO Handle -> IO ()
xtee i o = newEmptyMVar >>= \wait ->
           forkIO ((passThru (return stdin) o) >> putMVar wait ()) >>
           passThru i (return stdout) >> takeMVar wait

-- We want to copy input from one file handle to another unmodified (in text
-- mode). We can leave the buffering up to the Haskell runtime ({\sc ghc} will
-- use block buffering by default). Since |hGetContents| reads from the handle
-- (|i'|) lazily, this is very simple.

passThru :: IO Handle -> IO Handle -> IO ()
passThru i o = i >>= \i' -> o >>= \o' ->
               hGetContents i' >>= hPutStr o' >> hFlush o'

-- Now all we have to do is grab the input and output filenames from the
-- command-line and pass the open file handles to xtee.

main :: IO ()
main = getArgs >>= xteeOptions >>= \opts ->
       let inF     = head [ f | InFile  f <- opts ]
           outF    = head [ f | OutFile f <- opts ]
           inFile  = openFile inF  ReadMode
           outFile = openFile outF AppendMode
         in if DaemonMode `elem` opts
              then forever $ xtee inFile outFile
              else xtee inFile outFile
