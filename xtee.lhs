% Copyright 2008, 2009, 2010 Chris Forno
% This program is distributed under the terms of
% the GNU General Public License (version 3).

% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

\documentclass[oneside]{article}
%include polycode.fmt
\usepackage[T1]{fontenc}
\usepackage[x11names, rgb]{xcolor}
\usepackage[utf8]{inputenc}
\usepackage{tikz}
\usetikzlibrary{snakes,arrows,shapes}
\usepackage{amsmath}

\title{xtee 1.0}
\author{Chris Forno (jekor)}
\date{February 7th, 2010}

\begin{document}
\maketitle

\section{Basic Usage}

xtee (``cross-tee''/``expanded tee'') is a program for building complex
pipelines. It resembles the \verb!tee! command, except that instead of copying
stdin to stdout, it copies a file to stdout.

$$
\begin{tikzpicture}[>=latex,join=bevel,]
  \node (xtee) at (123bp,45bp) [draw,ellipse] {xtee};
  \node (stdin) at (30bp,72bp) [draw,ellipse] {stdin};
  \node (stdout) at (30bp,18bp) [draw,ellipse] {stdout};
  \node (infile) at (218bp,72bp) [draw,ellipse] {infile};
  \node (outfile) at (218bp,18bp) [draw,ellipse] {outfile};
  \draw [->] (stdin) ..controls (65bp,62bp) and (77bp,58bp)  .. (xtee);
  \draw [->] (xtee) ..controls (158bp,35bp) and (169bp,32bp)  .. (outfile);
  \draw [->] (infile) ..controls (181bp,62bp) and (169bp,58bp)  .. (xtee);
  \draw [->] (xtee) ..controls (88bp,35bp) and (77bp,32bp)  .. (stdout);
\end{tikzpicture}
$$

A simple demonstration of xtee:

\begin{verbatim}
tty1$ mkfifo /tmp/fifo-in /tmp/fifo-out
tty1$ echo "hi from tty1" > /tmp/fifo-in
tty2$ cat /tmp/fifo-out
tty3$ echo "hi from tty3" | xtee -i /tmp/fifo-in -o /tmp/fifo-out
\end{verbatim}

When you run the last command, \verb!hi from tty1! will appear on tty3 and
\verb!hi from tty3! on tty2.

\section{Example: Bidirectional {\sc Http} Filtering}

Suppose that Jim, a web developer, is working on his new client's website:
\mbox{xt-shirts.com}, a site decidated to extra large t-shirts. Jim doesn't
want to make changes to the live site without testing them locally first. So he
sets up a web server that emulates the live server and adds an entry for
\mbox{xt-shirts.com} to point to his test server.

This works OK for a while, but one day Jim begins testing the shopping cart. He
realizes that it's going to be difficult to keep track of which site's cookies
he has loaded in which browser (both the live and test site set cookies for the
domain \mbox{\verb!xt-shirts.com!}). Since the domain name is hard-coded all
throughout the code, it would take a lot of search and replace to have each
location read from a global domain name variable. Jim also knows that even
simple changes like that could have strange side-effects. And there's no
guarantees that another developer won't add more hard-coded domain names to the
code later.

Jim realizes that he can use a proxy to replace \mbox{\verb!xt-shirts.com!}
with\linebreak \mbox{\verb!xt-shirts.test!}. Then he won't have a problem with
cookies. And as a bonus, he won't ever get confused about whether he's viewing
the live site or the test version.

Jim is frustrated with all the {\sc http} proxies out there because they don't
support filtering in both directions and/or filtering the {\sc http} headers
(where the cookies are set, not to mention redirects!). After thinking about it
for a while, Jim comes up with the idea to combine some simple programs to make
the proxy himself:

$$
\begin{tikzpicture}[>=latex,join=bevel,scale=0.6]
  % Edge: browser -> netcat_in
  \draw [->] (69bp,39bp) .. controls (79bp,38bp) and (90bp,38bp)  .. (110bp,39bp);
  % Edge: netcat_in -> sed1
  \draw [->] (165bp,53bp) .. controls (175bp,56bp) and (186bp,59bp)  .. (206bp,65bp);
  % Edge: sed1 -> netcat_out
  \draw [->] (256bp,65bp) .. controls (265bp,62bp) and (276bp,59bp)  .. (297bp,53bp);
  % Edge: netcat_out -> server
  \draw [->] (352bp,39bp) .. controls (362bp,38bp) and (372bp,38bp)  .. (392bp,39bp);
  % Edge: server -> netcat_out
  \draw [->] (392bp,51bp) .. controls (382bp,52bp) and (372bp,52bp)  .. (352bp,51bp);
  % Edge: netcat_out -> sed2
  \draw [->] (297bp,37bp) .. controls (287bp,34bp) and (276bp,31bp)  .. (256bp,25bp);
  % Edge: sed2 -> netcat_in
  \draw [->] (206bp,25bp) .. controls (196bp,28bp) and (185bp,31bp)  .. (165bp,37bp);
  % Edge: netcat_in -> browser
  \draw [->] (110bp,51bp) .. controls (100bp,52bp) and (89bp,52bp)  .. (69bp,51bp);
  % Node: browser
  \draw (36bp,45bp) ellipse (35bp and 18bp);
  \draw (36bp,45bp) node {browser};
  % Node: netcat_in
  \draw (138bp,45bp) ellipse (30bp and 18bp);
  \draw (138bp,45bp) node {netcat};
  % Node: netcat_out
  \draw (324bp,45bp) ellipse (30bp and 18bp);
  \draw (324bp,45bp) node {netcat};
  % Node: server
  \draw (420bp,45bp) ellipse (30bp and 18bp);
  \draw (420bp,45bp) node {server};
  \pgfsetcolor{black}
  % Node: sed1
  \draw (231bp,72bp) ellipse (27bp and 18bp);
  \draw (231bp,72bp) node {sed};
  % Node: sed2
  \draw (231bp,18bp) ellipse (27bp and 18bp);
  \draw (231bp,18bp) node {sed};
\end{tikzpicture}
$$

The only problem is that netcat can't pass data into one program and return it
from another (i.e. into the first sed and back from the second). Using just 1
sed wouldn't work either for the same reason. Luckily, Jim knows of xtee and
refines his netcat pipeline to look like:

$$
\begin{tikzpicture}[>=latex,join=bevel,scale=0.6]
  % Edge: netcat_in -> xtee
  \draw [->] (58bp,39bp) .. controls (68bp,38bp) and (78bp,38bp)  .. (98bp,39bp);
  % Edge: xtee -> netcat_in
  \draw [->] (98bp,51bp) .. controls (88bp,52bp) and (78bp,52bp)  .. (58bp,51bp);
  % Edge: xtee -> fifo1
  \draw [->] (148bp,53bp) .. controls (158bp,56bp) and (168bp,59bp)  .. (188bp,65bp);
  % Edge: fifo1 -> sed1
  \draw [->] (240bp,72bp) .. controls (248bp,72bp) and (257bp,72bp)  .. (276bp,72bp);
  % Edge: sed1 -> fifo2
  \draw [->] (330bp,72bp) .. controls (338bp,72bp) and (347bp,72bp)  .. (366bp,72bp);
  % Edge: fifo2 -> netcat_out
  \draw [->] (418bp,65bp) .. controls (427bp,62bp) and (438bp,59bp)  .. (459bp,53bp);
  % Edge: netcat_out -> fifo3
  \draw [->] (459bp,37bp) .. controls (449bp,34bp) and (438bp,31bp)  .. (418bp,25bp);
  % Edge: fifo3 -> sed2
  \draw [->] (366bp,18bp) .. controls (358bp,18bp) and (349bp,18bp)  .. (330bp,18bp);
  % Edge: sed2 -> fifo4
  \draw [->] (276bp,18bp) .. controls (268bp,18bp) and (259bp,18bp)  .. (240bp,18bp);
  % Edge: fifo4 -> xtee
  \draw [->] (188bp,25bp) .. controls (179bp,28bp) and (168bp,31bp)  .. (148bp,37bp);
  % Node: netcat_in
  \draw (30bp,45bp) ellipse (30bp and 18bp);
  \draw (30bp,45bp) node {netcat};
  % Node: xtee
  \draw (123bp,45bp) ellipse (27bp and 18bp);
  \draw (123bp,45bp) node {xtee};
  % Node: netcat_out
  \draw (486bp,45bp) ellipse (30bp and 18bp);
  \draw (486bp,45bp) node {netcat};
  % Node: sed1
  \draw (303bp,72bp) ellipse (27bp and 18bp);
  \draw (303bp,72bp) node {sed};
  % Node: sed2
  \draw (303bp,18bp) ellipse (27bp and 18bp);
  \draw (303bp,18bp) node {sed};
  % Node: fifo1
  \draw (213bp,72bp) ellipse (27bp and 18bp);
  \draw (213bp,72bp) node {fifo1};
  % Node: fifo4
  \draw (213bp,18bp) ellipse (27bp and 18bp);
  \draw (213bp,18bp) node {fifo4};
  % Node: fifo2
  \draw (393bp,72bp) ellipse (27bp and 18bp);
  \draw (393bp,72bp) node {fifo2};
  % Node: fifo3
  \draw (393bp,18bp) ellipse (27bp and 18bp);
  \draw (393bp,18bp) node {fifo3};
\end{tikzpicture}
$$

Jim decides to test the idea on the test web server. First, he sets the {\sc
http} server to only listen for connections on port 80 of the loopback
interface (127.0.0.1). Next, he runs the following commands on the test server.

\begin{verbatim}
tty1$ mkfifo /tmp/fifo1 /tmp/fifo2 /tmp/fifo3 /tmp/fifo4
tty1$ sed -e 's/xt-shirts\.lan/xt-shirts.com/g' < /tmp/fifo1 \
      > /tmp/fifo2
tty2$ nc 127.0.0.1 80 < /tmp/fifo2 > /tmp/fifo3
tty3$ sed -e 's/xt-shirts\.com/xt-shirts.lan/g' < /tmp/fifo3 \
      > /tmp/fifo4
tty4$ nc -l -p 80 10.0.0.3 -c "xtee -o /tmp/fifo1 -i /tmp/fifo4"
\end{verbatim}

He then adds 10.0.0.3 (the address of the test server) to the \verb!/etc/hosts!
file on his computer (the one he uses his web browser from) and points his
browser to \mbox{\verb!xt-shirts.lan!}...and it works!

Note that Jim uses the \verb!.lan! suffix instead of \verb!.test! so that he
doesn't change the \verb!Content-Length! of the body of any {\sc http}
transactions (which might confuse web browsers).

The only problem with the setup is that xtee and the first sed exit after each
request in the {\sc http} session, but he sets up a script to respawn them.
\footnote{The reason that xtee terminates is because it sees the \verb!EOF!
marker at the end of the {\sc http} request. Once xtee exits, the first sed
sees the end of its input and exits as well. Each netcat will remain running
for the entire {\sc http} session because that's how netcat was designed. (They
also keep the second sed running.) Jim makes a note to request a ``daemon''
mode from the xtee author.}

\section{Source Code}

> import System.IO
> import System.Console.GetOpt
> import System.Environment
> import System.Exit
> import Control.Concurrent
> import Control.Monad

> version  :: Double
> version  =  1.0

> usage    :: String
> usage    =  "Usage: xtee [option...] -i input-file -o output-file"

All of the following is just for handling commandline options.

> data Flag = Help | Version | InFile String | OutFile String | DaemonMode
>             deriving (Show, Eq)

> options :: [OptDescr Flag]
> options =  [
>  Option ['h']  ["help"]     (NoArg Help)              "show help",
>  Option ['v']  ["version"]  (NoArg Version)           "show version",
>  Option ['i']  ["input"]    (ReqArg InFile   "FILE")  "input file",
>  Option ['o']  ["output"]   (ReqArg OutFile  "FILE")  "output file",
>  Option ['d']  ["daemon"]   (NoArg DaemonMode)        "daemon mode" ]

> xteeOptions      :: [String] -> IO ([Flag])
> xteeOptions argv =
>    case getOpt Permute options argv of
>       ([],  _,[])    ->  ioError (userError ("\n" ++ showUsage))
>       (o,   _,[])    ->  if Help `elem` o then 
>                               putStr showUsage >> exitWith ExitSuccess
>                          else
>                              if Version `elem` o then
>                                  putStr (showVersion ++ "\n") >> exitWith ExitSuccess
>                              else
>                                  return o
>       (_,   _,errs)  ->  ioError (userError (concat errs ++ showUsage))
>   where  showVersion   =  "xtee version " ++ show version
>          usageHeader   =  unlines [ showVersion, usage ]
>          showUsage     =  usageInfo usageHeader options

xtee copies its stdin to a file and copies another file to its stdout.
We first spawn a process (a Haskell process, not an {\sc os} process) to pass stdin
through to the output file and then we pass the input file though to stdout. To
make sure that both complete before we exit, we use the \verb!wait! MVar.

> xtee      ::  IO Handle -> IO Handle -> IO ()
> xtee i o  =   newEmptyMVar >>= \wait ->
>               forkIO ((passThru (return stdin) o) >> putMVar wait ()) >>
>               passThru i (return stdout) >> takeMVar wait

We want to copy input from one file handle to another unmodified (in text
mode). We can leave the buffering up to the Haskell runtime ({\sc ghc} will use
block buffering by default). Since |hGetContents| reads from the handle (|i'|)
lazily, this is very simple.

> passThru     :: IO Handle -> IO Handle -> IO ()
> passThru i o =  i >>= \i' -> o >>= \o' ->
>                 hGetContents i' >>= hPutStr o' >> hFlush o'

Now all we have to do is grab the input and output filenames from the
command\-line and pass the open file handles to |xtee|.

> main  :: IO ()
> main  =  getArgs >>= xteeOptions >>= \opts ->
>          let  inF      = head [ f | InFile   f <- opts ]
>               outF     = head [ f | OutFile  f <- opts ]
>               inFile   = openFile inF   ReadMode
>               outFile  = openFile outF  AppendMode
>          in if DaemonMode `elem` opts
>             then forever $ xtee inFile outFile
>             else xtee inFile outFile

\end{document}