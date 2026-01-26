#!/usr/bin/env -S runghc --ghc-arg=-x --ghc-arg=lhs

4-ch via The Internet Gopher Protocol
=====================================

Browse this early 2000s text board like it's the early 90s.

https://4-ch.net/

This is a Literate Haskell script intended to be used as a "gopher applet," or,
in other words, a script that should be executed by a Gopher daemon like
[Venusia](https://github.com/someodd/venusia).

This is basically a specialized HTTP <-> Gopher proxy. The trickiest part is the
handling of the cookie-based captcha system for posting.

Everything is kept as minimal and simple as possible, with certain features
intentionally omitted. I try not to use dependencies. This also means I am
using regex to parse HTML (yes, I know).

Currently is read-only (4-ch viewer), but I hope to one day add a posting mode.

Made kind of as a joke because I read [a post](https://4-ch.net/tech/kareha.pl/1721174972) that said (edited):

    4-ch isn't accessible from gopher, gemini, or whatever ph@#$%^ protocol
    sissy hippies who didnt live in the past but want to live in the past are
    obsessed with at the moment and therefore makes those protocols useless.

Much love and lulz to this poster.

This is currently running at: gopher://gopher.someodd.zip/1/gateway/4_ch

Installing Dependencies
~~~~~~~~~~~~~~~~~~~~~~~

The script uses basically no dependencies, save things I assume you'd already
have installed (beyond `ghc`). This way it can be easily run with `runghc` as a
script. I go out of my way to avoid non-base/standard dependencies.

Nonetheless, just in case, here's how to install some dependencies.

```
sudo apt install curl ghc
```

Imports
-------

This should give you an idea of the kind of work we will be doing. The
libraries we use. Since I don't want to include any Haskell dependencies we
will rely a lot on calling upon the standard Linux tools:

> import System.Process (readProcess)

This is a CLI script, after all:

> import System.Environment (getArgs)

... then the rest:

> import Data.List (intercalate)

Config
------

Configuration constants will go here.

First is the root selector (think path) we assume will be the prefix in all
Gopher requests to this script. This is used when writing menus:

> baseSelector :: String
> baseSelector = "/gateway/4_ch"

Sub-selector for viewing a specific board (no slashes):

> subSelectorViewBoard :: String
> subSelectorViewBoard = "view_threads"

Sub-selector for viewing a specific thread on a board (no slashes):

> subSelectorViewThread :: String
> subSelectorViewThread = "view_thread"

The host and port this script is running on, used for writing menus (so we know how to link to ourselves):

> host :: String
> host = "gopher.someodd.zip"
>
> port :: Int
> port = 70

Helpers
-------

These are just re-used utility functions which assist in the interface.

Firstly, let's create data type for a correctly formatted line in the expected
[RFC 1436](https://www.rfc-editor.org/rfc/rfc1436) Gopher menu item
specification format. That format is:

```
<item char><label>\t<selector>\t<host>\t<port>
```

... here's the actual data type representation:

> data GopherLine = GopherLine Char String String String Int

Here's a function that will transform a `GopherLine` to an actual `String`:

> lineAsText :: GopherLine -> String
> lineAsText (GopherLine itemChar label selector host port) = intercalate "\t" [itemChar : label, selector, host, show port]

Now here's a function that can turn a bunch of lines into a ready-to-transmit Gopher menu:

> gopherMenu :: [GopherLine] -> String
> gopherMenu gopherLines = intercalate "\n" (map lineAsText gopherLines)

Something we'll often do is turn lines into a menu and then print the results:

> putLines :: [GopherLine] -> IO ()
> putLines = putStrLn . gopherMenu

Smash a selector's parts together:

> makeSelector :: [String] -> String
> makeSelector = intercalate "/"

TSV Parsing (Output From Our `curl | sed` Commands
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We make extensive use of tab-delimited results when we use `curl` (and then
pipe to `sed`) to prepare results for parsing. The idea of these helpers is to
encourage standardization of the command output.

Sometimes the prepared TSV is more than two fields, thus this function:

> splitString :: Char -> String -> [String]
> splitString _ "" = [""]
> splitString delimiter str = 
>     let (start, rest) = break (== delimiter) str
>     in start : case rest of
>         "" -> []
>         (_:remain) -> splitString delimiter remain

Splits string by the specific token ` \n `, which is helpful when parsing multi-line content:

> splitByNewlineToken :: String -> [String]
> splitByNewlineToken "" = []
> splitByNewlineToken str = 
>     let token = " \\n "
>         (start, rest) = breakSubstring token str
>     in start : if null rest then [] else splitByNewlineToken (drop (length token) rest)

Break a string at a specific substring:

> breakSubstring :: String -> String -> (String, String)
> breakSubstring pattern [] = ("", "")
> breakSubstring pattern str@(x:xs)
>     | take (length pattern) str == pattern = ("", str)
>     | otherwise = 
>         let (pre, post) = breakSubstring pattern xs
>         in (x : pre, post)

This is redundant, or at least should be using `splitString '\t'`:

> splitByTab :: String -> [String]
> splitByTab "" = []
> splitByTab str = 
>     let (start, rest) = break (== '\t') str
>     in start : case rest of
>         [] -> []
>         (_:r) -> splitByTab r

Get List of Boards
------------------

Firstly, we will get the list of boards using a `curl` request to
`https://4-ch.net/side.html`, outputting a menu of links to other menus.

This should be exposed as `baseSelector`.

> fetchBoards :: IO ()
> fetchBoards = do
>   let cmd = "curl -s https://4-ch.net/side.html | sed -n 's|.*href=\"https://4-ch.net/\\([a-z0-9]\\+\\)/\"[^>]*>\\([^<]\\+\\)</a>.*|\\1\\t\\2|p'"
>   output <- readProcess "bash" ["-c", cmd] ""
>   let rawLines = lines output
>   putLines $ map parseBoardLine rawLines

List of Boards Helper
~~~~~~~~~~~~~~~~~~~~~
This is like a sub-function that parses the essentially CSV result of the
`curl` request, turning a result into an actual line for a Gopher menu
response:

> parseBoardLine :: String -> GopherLine
> parseBoardLine line = case break (== '\t') line of

... so for the first match, either we get a proper board match (URL <-> board name):

>     (b, '\t':lbl) -> GopherLine '1' lbl (makeSelector [baseSelector, subSelectorViewBoard, b]) host port

... or we don't because of some kind of error:

>     _        -> GopherLine '3' ("Error parsing: " ++ line) baseSelector host port

View a Specific Board
=====================

Please expose this as `baseSelector/subSelectorViewBoard/*` where `*` is a
wildcard that matches for the name of the board to view.

This will perform a `curl` request to, for example: https://4-ch.net/tech/

The returned result is simply a list of menu items, which are thread titles,
their reply count, linking to a view thread menu.

The actual command which will display the threads as a Gopher menu:

> fetchThreads :: String -> IO ()
> fetchThreads boardName = do
>     let url = "https://4-ch.net/" ++ boardName ++ "/"
>     let cmd = "curl -s --compressed " ++ url ++ " | tr -d '\\n' | " ++
>               "sed 's/<\\/h2>/\\n/g' | grep \"kareha.pl\" | " ++
>               "sed -n 's/&amp;/\\&/g; s/&#39;/'\\''/g; s/&#44;/,/g; " ++
>               "s/.*href=\"\\([^\"]\\+\\)\".*>\\(.*\\)<small>(\\([0-9]\\+\\)).*/\\1\\t\\2\\t\\3/p'"
> 
>     output <- readProcess "bash" ["-c", cmd] ""
>     
>     let rawLines = lines output
>     if null rawLines
>         then putLines [GopherLine '3' ("Error: No threads found for " ++ boardName) "fake" "0" 0]
>         else putLines $ map (parseThreadLine boardName) rawLines

View Board Helpers
~~~~~~~~~~~~~~~~~~
This is a helper to parse a result from the wild curl/sed command used to fetch threads:

> parseThreadLine :: String -> String -> GopherLine
> parseThreadLine boardName line = 
>     case break (== '\t') line of
>         (link, '\t':rest) -> 
>             case break (== '\t') rest of
>                 (title, '\t':count) -> 
>                     let threadId = extractId link
>                         cleanCount = filter (/= '\t') count
>                         displayTitle = title ++ " (" ++ cleanCount ++ ")"
>                         selector = makeSelector [baseSelector, subSelectorViewThread, boardName, threadId]
>                     -- Uses global 'host' and 'port' here
>                     in GopherLine '1' displayTitle selector host port
>                 _ -> GopherLine '3' ("Error parsing title/count: " ++ line) "fake" "0" 0
>         _ -> GopherLine '3' ("Error parsing link: " ++ line) "fake" "0" 0

Another helper to assist with parsing:

> extractId :: String -> String
> extractId url = 
>     let parts = splitString '/' url
>         isId s = length s > 5 && all (`elem` "0123456789") s
>     in case filter isId parts of
>         (id:_) -> id
>         []     -> "0"


View a Specific Thread
======================

View a thread on a specific board.

Please expose this as `baseSelector/subSelectorViewThread/*` where `*` is a wildcard that's matching something like `boardslug/threadid`.

This will perform a `curl` request to, for example: https://4-ch.net/tech/kareha.pl/1768665091

> fetchThread :: String -> String -> IO ()
> fetchThread boardName threadId = do
>     let url = "https://4-ch.net/" ++ boardName ++ "/kareha.pl/" ++ threadId
>     -- Note: In the Haskell string, we need quadruple backslashes to get a double backslash in the shell command
>     let cmd = "curl -s --compressed " ++ url ++ " | tr -d '\\n' | " ++
>               "sed 's/<div class=\"reply\"/\\nPOST/g' | " ++
>               "sed -n 's|<form.*||g; s|<div id=\"footer\".*||g; " ++
>               "/POST/! s|.*<h2>\\([^<]*\\)<small>.*|HEAD\\t\\1|p; " ++
>               "/^POST/ { s|<div class=\"replytext\">|\\t|; " ++
>               "s|.*postername\"[^>]*>\\([^<]*\\).*</span> : \\(.*\\) <span class=\"deletebutton\".*\\t\\(.*\\)</div> </div>.*|POST\\t\\1\\t\\2\\t\\3|; " ++
>               "t c; d; :c; s|<br />| \\\\n |g; s|</p>| \\\\n\\\\n |g; s|<blockquote>|> |g; s|<[^>]*>||g; " ++
>               "s/&gt;/>/g; s/&lt;/</g; s/&quot;/\"/g; s/&amp;/\\&/g; s/&#39;/'\\''/g; s/&#44;/,/g; p }'"
>
>     output <- readProcess "bash" ["-c", cmd] ""
>     
>     -- Define Navigation
>     let returnSelector = makeSelector [baseSelector, subSelectorViewBoard, boardName]
>     let returnLink = GopherLine '1' ("<-- Return to /" ++ boardName ++ "/") returnSelector host port
>     let spacer = GopherLine 'i' " " "fake" "0" 0
>     
>     -- Parse Content
>     let content = concatMap (parsePostLine boardName threadId) (lines output)
>     
>     -- Output: [Link, Spacer] + Content + [Spacer, Link]
>     putLines $ [returnLink, spacer] ++ content ++ [spacer, returnLink]

View Thread Helper(s)
~~~~~~~~~~~~~~~~~~~~~
This handles nicely formatting (for a Gopher menu) the TSV result from the wild
`curl` to `sed` pipe:

> parsePostLine :: String -> String -> String -> [GopherLine]
> parsePostLine boardName threadId line 
>     | take 4 line == "HEAD" =
>         let title = drop 5 line
>         in [ GopherLine 'i' ("--- " ++ title ++ " ---") "fake" "0" 0
>            , GopherLine 'i' " " "fake" "0" 0 ]
>     | take 4 line == "POST" =
>         case splitByTab line of
>             ("POST":name:date:content:[]) ->
>                 let header = name ++ " (" ++ date ++ ")"
>                     -- Split content by the literal string " \n "
>                     bodyLines = splitByNewlineToken content
>                     gopherBody = map (\l -> GopherLine 'i' ("  " ++ l) "fake" "0" 0) bodyLines
>                 in [ GopherLine 'i' header "fake" "0" 0 ] ++ gopherBody ++ [ GopherLine 'i' " " "fake" "0" 0 ]
>             _ -> []
>     | otherwise = []
>

Entrypoint
==========

This is the function which is executed first when this script is ran, essentially:

> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         ["boards"]       -> fetchBoards
>         ["threads", b]   -> fetchThreads b
>         ["thread", path] -> case break (== '/') path of
>             (b, '/':tid) -> fetchThread b tid
>             _            -> putStrLn "Error: expected format 'board/threadid'"
>         _                -> putStrLn "Usage: boards | threads [board] | thread [board/id]"
