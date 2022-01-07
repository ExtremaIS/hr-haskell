{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HR.Mock (tests) where

-- https://hackage.haskell.org/package/HMock
import qualified Test.HMock as HMock
import Test.HMock ((|->))

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit (testCase)

-- (hr)
import qualified HR
import HR.Monad.Terminal (MonadTerminal(..))

------------------------------------------------------------------------------

HMock.makeMockable [t|MonadTerminal|]

------------------------------------------------------------------------------

parts :: HR.Parts
parts = HR.Parts
    { HR.leftPart  = "══╣"
    , HR.midPart   = "╠═╣"
    , HR.rightPart = "╠══"
    , HR.fillPart  = '═'
    }

------------------------------------------------------------------------------

testPutAscii :: TestTree
testPutAscii = testCase "putAscii" . HMock.runMockT $ do
    HMock.expect $ PutStrLn "--|test|------------" |-> ()
    HR.putAscii 20 ["test"]

------------------------------------------------------------------------------

testPutUnicode :: TestTree
testPutUnicode = testCase "putUnicode" . HMock.runMockT $ do
    HMock.expect $ PutStrLn "━━┫test┣━━━━━━━━━━━━" |-> ()
    HR.putUnicode 20 ["test"]

------------------------------------------------------------------------------

testPut :: TestTree
testPut = testCase "put" . HMock.runMockT $ do
    HMock.expect $ PutStrLn "══╣test╠════════════" |-> ()
    HR.put parts 20 ["test"]

------------------------------------------------------------------------------

testPutAutoAscii :: TestTree
testPutAutoAscii = testGroup "putAutoAscii"
    [ testCase "auto" . HMock.runMockT $ do
        HMock.expect $ GetWidth |-> Just 20
        HMock.expect $ PutStrLn "--|test|------------" |-> ()
        HR.putAutoAscii 30 ["test"]
    , testCase "default" . HMock.runMockT $ do
        HMock.expect $ GetWidth |-> Nothing
        HMock.expect $ PutStrLn "--|test|--" |-> ()
        HR.putAutoAscii 10 ["test"]
    ]

------------------------------------------------------------------------------

testPutAutoUnicode :: TestTree
testPutAutoUnicode = testGroup "putAutoUnicode"
    [ testCase "auto" . HMock.runMockT $ do
        HMock.expect $ GetWidth |-> Just 20
        HMock.expect $ PutStrLn "━━┫test┣━━━━━━━━━━━━" |-> ()
        HR.putAutoUnicode 30 ["test"]
    , testCase "default" . HMock.runMockT $ do
        HMock.expect $ GetWidth |-> Nothing
        HMock.expect $ PutStrLn "━━┫test┣━━" |-> ()
        HR.putAutoUnicode 10 ["test"]
    ]

------------------------------------------------------------------------------

testPutAuto :: TestTree
testPutAuto = testGroup "putAuto"
    [ testCase "auto" . HMock.runMockT $ do
        HMock.expect $ GetWidth |-> Just 20
        HMock.expect $ PutStrLn "══╣test╠════════════" |-> ()
        HR.putAuto parts 30 ["test"]
    , testCase "default" . HMock.runMockT $ do
        HMock.expect $ GetWidth |-> Nothing
        HMock.expect $ PutStrLn "══╣test╠══" |-> ()
        HR.putAuto parts 10 ["test"]
    ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "HR:Mock"
    [ testPutAscii
    , testPutUnicode
    , testPut
    , testPutAutoAscii
    , testPutAutoUnicode
    , testPutAuto
    ]
