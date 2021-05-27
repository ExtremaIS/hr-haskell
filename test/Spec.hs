module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (hr:test)
import qualified HR.Test

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test"
    [ HR.Test.tests
    ]
