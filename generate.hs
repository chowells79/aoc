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

main :: IO ()
main = do
    [yearS, dayS] <- getArgs
    let year = read yearS
        day = mkDay_ (read dayS)
    sessionKeyRaw <- readFile ".session_key"
    let sessionKey = filter (not . isSpace) sessionKeyRaw
        userAgent = AoCUserAgent "private" "chowells79@gmail.com"
        opts = defaultAoCOpts userAgent year sessionKey

    released <- challengeReleased year day
    case released of
        True -> do
            putStrLn "Fetching input..."
            input <- runAoC_ opts $ AoCInput day
            let fileName = printf "%d/input/%02d.txt" year (dayInt day)
            T.writeFile fileName input
            putStrLn "Input fetched."
        False -> putStrLn "Not released, skipping input."

    let ident = printf "%02d" (dayInt day)
        outName = printf "%d/Day%s.hs" year ident

    alreadyExists <- fileExist outName
    case alreadyExists of
        True -> putStrLn "File already exists. Not overwriting."
        False -> do
            tpl <- T.readFile "DayN.hs.tpl"
            let ctx "id" = T.pack ident
                ctx t = error $ "Unknown variable: " ++ T.unpack t

                out = TT.substitute tpl ctx

            TL.writeFile outName out

            putStrLn "File created"

            setFileMode outName $ foldr unionFileModes nullFileMode [
                ownerModes, groupReadMode, groupExecuteMode,
                otherReadMode, otherExecuteMode ]
