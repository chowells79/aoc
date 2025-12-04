#!/usr/bin/env cabal
{- cabal:
build-depends: base, advent-of-code-api, text, template, unix
-}
{-# Language OverloadedStrings #-}

import System.Environment

import Advent

import Data.Char

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import Text.Printf

import Data.Foldable
import Control.Monad

import qualified Data.Text.Template as TT

import System.Posix.Files
import System.Posix.Directory
import System.Posix.Types

main :: IO ()
main = do
    [yearS, dayS] <- getArgs
    let year = read yearS
        day = mkDay_ (read dayS)

    makeSolution year day
    makeExample year day
    fetchInput year day


fetchInput :: Integer -> Day -> IO ()
fetchInput year day = do
    sessionKeyRaw <- readFile ".session_key"
    let sessionKey = filter (not . isSpace) sessionKeyRaw
        userAgent = AoCUserAgent "https://github.com/chowells79/aoc" "chowells79@gmail.com"
        opts = defaultAoCOpts userAgent year sessionKey

    released <- challengeReleased year day
    case released of
        True -> do
            putStrLn "Fetching input..."
            input <- runAoC_ opts $ AoCInput day
            let dirName = printf "%d/input" year
                fileName = printf "%s/%02d.txt" dirName (dayInt day)
            ensureDir dirName
            T.writeFile fileName input
            setFileMode fileName createdMode
            putStrLn "Input fetched."
        False -> putStrLn "Not released, skipping input."

makeSolution :: Integer -> Day -> IO ()
makeSolution year day = do
    let ident = printf "%02d" (dayInt day)
        yearDir = printf "%d" year
        outName = printf "%s/Day%s.hs" yearDir ident

    ensureDir yearDir
    alreadyExists <- fileExist outName
    case alreadyExists of
        True -> putStrLn $
            "Solution file " ++ outName ++ " already exists. Not overwriting."
        False -> do
            tpl <- T.readFile "DayN.hs.tpl"
            let ctx "id" = T.pack ident
                ctx t = error $ "Unknown variable: " ++ T.unpack t

                out = TT.substitute tpl ctx

            TL.writeFile outName out
            setFileMode outName createdMode

            putStrLn "Solution file created"


makeExample :: Integer -> Day -> IO ()
makeExample year day = do
    let exampleDir = printf "%d/example" year
        outName = printf "%s/%02d-1.txt" exampleDir (dayInt day)

    ensureDir exampleDir
    alreadyExists <- fileExist outName
    case alreadyExists of
        True -> putStrLn $
            "Example input " ++ outName ++ " already exists. Not overwriting."
        False -> do
            T.writeFile outName $ T.pack ""
            setFileMode outName createdMode
            putStrLn "(Empty) example input created"

ensureDir :: String -> IO ()
ensureDir dirName = do
    alreadyExists <- fileExist dirName
    case alreadyExists of
        True -> pure ()
        False -> do
            putStrLn $ "Creating directory " ++ dirName
            createDirectory dirName createdMode

createdMode :: FileMode
createdMode = foldl' unionFileModes nullFileMode
    [ ownerModes
    , groupReadMode, groupExecuteMode
    , otherReadMode, otherExecuteMode
    ]
