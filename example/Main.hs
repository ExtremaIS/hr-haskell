{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- https://github.com/ExtremaIS/hr-haskell
import qualified HR

------------------------------------------------------------------------------

main :: IO ()
main = HR.putAutoUnicode 80 ["example"]
