{-# LANGUAGE CPP #-}

module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (hr:test)
import qualified HR.Test
#if __GLASGOW_HASKELL__ >= 806
import qualified HR.Mock
#endif

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test"
    [ HR.Test.tests
#if __GLASGOW_HASKELL__ >= 806
    , HR.Mock.tests
#endif
    ]
