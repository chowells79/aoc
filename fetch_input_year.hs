#!/usr/bin/env cabal
{- cabal:
build-depends: base, advent-of-code-api, text
-}
{-# Language OverloadedStrings #-}

import System.Environment

import Advent

import Data.Char

import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as T

import Text.Printf

import Data.Foldable
import Control.Monad

main :: IO ()
main = do
    [yearS] <- getArgs
    let year = read yearS
    sessionKeyRaw <- readFile ".session_key"
    let sessionKey = filter (not . isSpace) sessionKeyRaw
        userAgent = AoCUserAgent "private" "chowells79@gmail.com"
        opts = defaultAoCOpts userAgent year sessionKey
        maxDay | year < 2025 = 25
               | otherwise = 12
    forM_ [ mkDay_ 1 .. mkDay_ maxDay ] $ \day -> do
        released <- challengeReleased year day
        when released $ do
            input <- runAoC_ opts $ AoCInput day
            let fileName = printf "%d/input/%02d.txt" year (dayInt day)
            T.writeFile fileName input

