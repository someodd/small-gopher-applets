#!/usr/bin/env -S runghc --ghc-arg=-x --ghc-arg=lhs

ChatGopherPT: Ollama ↔ Gopher Glue (KISS)
========================================

This script is the glue between Venusia (or any gopher daemon that can run commands)
and Ollama.

It provides two entry points:

- `list`  : show installed models as a gopher menu
- `<model> <prompt...>` : run a model with a prompt

And it enforces one extremely important rule:

Only one request may run at a time
----------------------------------

This prevents your home server from being hammered.

We implement "single-flight" locking using an atomic filesystem operation:

- Acquire lock by creating a directory (atomic on POSIX).
- If it already exists, someone else is running: return a "busy" message.
- Always release the lock afterward (even on errors) via `finally`.

The only remaining failure mode is a hard crash / `kill -9`,
which may leave the lock directory behind.

In that case, remove it manually:

    sudo rm -rf /tmp/chatgopherpt-ollama.lock.d


Setup
=====

1) Install GHC (needed for `runghc`):

    sudo apt install ghc

2) Place this script somewhere executable, e.g.:

    /var/gopher/output/chatgopherpt

3) Make it executable:

    sudo chmod +x /var/gopher/output/chatgopherpt


Venusia routes.toml
===================

Use one script for both "list" and "run":

    [[gateway]]
    selector = "/gateway/ollama/list"
    search = false
    wildcard = false
    command = "/var/gopher/output/chatgopherpt"
    arguments = ["list"]
    menu = true

    [[gateway]]
    selector = "/gateway/ollama/*"
    search = true
    wildcard = true
    command = "/var/gopher/output/chatgopherpt"
    arguments = ["$wildcard", "$search"]
    menu = false

Why `menu=false` for /gateway/ollama/* ?
---------------------------------------

`ollama run` returns plain text (often with code blocks). That is not a gopher menu.
So we treat it as text.

When the server is busy, this script prints a short *menu-style* response anyway.
If the client renders it as text, that's fine — it's still readable.


Code
====

Imports
-------

We keep imports minimal: base + process + directory + exception.

> import Control.Exception (SomeException, try, finally)
> import Data.Char (isPrint)
> import Data.List (intercalate)
> import System.Environment (getArgs)
> import System.Process (readProcess, callProcess)
> import System.Directory (createDirectory, removeDirectoryRecursive)
> import System.IO (hSetBuffering, BufferMode(..), stdout)

Configuration
-------------

- `lockDir` is the "only one at a time" mutex.
- `gopherHost`/`gopherPort` are used only for the `list` menu links.

> lockDir :: FilePath
> lockDir = "/tmp/chatgopherpt-ollama.lock.d"
>
> gopherHost :: String
> gopherHost = "gopher.someodd.zip"
>
> gopherPort :: String
> gopherPort = "70"

Main dispatch
-------------

The script is invoked in exactly two shapes:

- `chatgopherpt list`
- `chatgopherpt <model> <prompt...>`

> main :: IO ()
> main = do
>   -- Gopher is line-oriented; avoid buffering surprises.
>   hSetBuffering stdout NoBuffering
>   args <- getArgs
>   case args of
>     ["list"]     -> cmdList
>     (m:rest)     -> cmdRun m (normalize rest)
>     _            -> infoMenu
>                      [ "Usage:"
>                      , "  /gateway/ollama/list"
>                      , "  /gateway/ollama/<model>?<prompt>"
>                      ]

Prompt handling
---------------

Venusia may pass `$search` as one argument or multiple; we normalize by joining.

> normalize :: [String] -> String
> normalize []  = ""
> normalize [x] = x
> normalize xs  = intercalate " " xs

Gopher output helpers
---------------------

We only need two output styles:

- Info menu lines: item type `i`
- Search items: item type `7` (client prompts the user for a query)

We also sanitize tabs/newlines because gopher menu lines are tab-delimited.

> sanitize :: String -> String
> sanitize =
>   map (\c -> if c == '\t' || c == '\r' || c == '\n'
>              then ' '
>              else if isPrint c then c else '?')
>
> infoMenu :: [String] -> IO ()
> infoMenu ls = do
>   mapM_ (\s -> putStrLn ("i" ++ sanitize s ++ "\tfake\tfake\t0")) ls
>   putStrLn "."
>
> searchItem :: String -> String -> IO ()
> searchItem label selector =
>   putStrLn ("7" ++ sanitize label ++ "\t" ++ selector ++ "\t" ++ gopherHost ++ "\t" ++ gopherPort)

Command: list
-------------

We run `ollama list`, parse the first column after the header, and print entries
that point at `/gateway/ollama/<model>`.

> cmdList :: IO ()
> cmdList = do
>   out <- readProcess "ollama" ["list"] ""
>   let models = parseList out
>   if null models
>     then infoMenu
>            [ "No models found."
>            , "Try: ollama pull deepseek-r1:14b"
>            ]
>     else do
>       mapM_ (\m -> searchItem ("Use model " ++ m) ("/gateway/ollama/" ++ m)) models
>       putStrLn "."
>
> parseList :: String -> [String]
> parseList s =
>   let ls = lines s
>       body = drop 1 ls  -- skip header row
>       firstCol line =
>         takeWhile (not . (`elem` [' ', '\t']))
>           (dropWhile (`elem` [' ', '\t']) line)
>   in filter (not . null) (map firstCol body)

Command: run
------------

This is the main entry point used by `/gateway/ollama/*`.

Steps:

1) Validate model + prompt
2) Acquire lock (single-flight)
3) If busy, print the friendly "home server" message
4) Otherwise, run: `ollama run <model> <prompt>`
5) Always release lock

> cmdRun :: String -> String -> IO ()
> cmdRun model prompt =
>   if null model || null prompt
>     then infoMenu
>            [ "Missing model or prompt."
>            , "Pick a model from /gateway/ollama/list"
>            ]
>     else do
>       locked <- acquireLock
>       if not locked
>         then infoMenu
>                [ "Hold up — someone else is using this right now."
>                , "This is hosted on a shitty little server in my home."
>                , "Try again in a moment."
>                ]
>         else
>           (callProcess "ollama" ["run", model, prompt])
>             `finally` releaseLock

Locking (the whole point)
-------------------------

KISS locking via `createDirectory`:

- Succeeds only for one process (atomic).
- Fails for everyone else.
- No polling, no queues.

> acquireLock :: IO Bool
> acquireLock = do
>   r <- try (createDirectory lockDir) :: IO (Either SomeException ())
>   pure (either (const False) (const True) r)
>
> releaseLock :: IO ()
> releaseLock = do
>   _ <- try (removeDirectoryRecursive lockDir) :: IO (Either SomeException ())
>   pure ()

