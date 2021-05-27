------------------------------------------------------------------------------
-- |
-- Module      : HR
-- Description : horizontal rule for terminals
-- Copyright   : Copyright (c) 2019-2021 Travis Cardwell
-- License     : MIT
--
-- This library is meant to be imported qualified, as follows:
--
-- @
-- import qualified HR
-- @
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HR
  ( -- * Constants
    version
    -- * Parts
  , Parts(..)
  , asciiParts
  , unicodeParts
    -- * API
    -- ** Pure
  , render
  , renderAscii
  , renderUnicode
    -- ** IO
  , put
  , putAscii
  , putUnicode
  , putAuto
  , putAutoAscii
  , putAutoUnicode
  ) where

-- https://hackage.haskell.org/package/base
import Data.Version (showVersion)

-- https://hackage.haskell.org/package/terminal-size
import qualified System.Console.Terminal.Size as Terminal

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO

-- (hr:cabal)
import qualified Paths_hr as Project

------------------------------------------------------------------------------
-- $Constants

-- | Package version
version :: String
version = "hr-haskell " ++ showVersion Project.version

------------------------------------------------------------------------------
-- $Parts

-- | Text parts of a horizontal rule
--
-- @since 0.3.0.0
data Parts
  = Parts
    { leftPart  :: !Text  -- ^ text before the first note
    , midPart   :: !Text  -- ^ text between notes
    , rightPart :: !Text  -- ^ text after the last note
    , fillPart  :: !Char  -- ^ fill character
    }

-- | ASCII text parts of a horizontal rule
--
-- @since 0.3.0.0
asciiParts :: Parts
asciiParts = Parts
    { leftPart  = "--|"
    , midPart   = "|-|"
    , rightPart = "|--"
    , fillPart  = '-'
    }

-- | Unicode text parts of a horizontal rule
--
-- @since 0.3.0.0
unicodeParts :: Parts
unicodeParts = Parts
    { leftPart  = "━━┫"
    , midPart   = "┣━┫"
    , rightPart = "┣━━"
    , fillPart  = '━'
    }

------------------------------------------------------------------------------
-- $API

-- | Render a horizontal rule
--
-- Note that the rendered horizontal rule maybe longer than the specified rule
-- width if the provided notes is too wide.
--
-- @since 0.3.0.0
render
  :: Parts
  -> Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> Text
render Parts{..} width = \case
    [] -> T.pack $ replicate width fillPart
    notes ->
      let rule = T.concat
            [ leftPart
            , T.intercalate midPart notes
            , rightPart
            ]
      in  case max 0 (width - T.length rule) of
            0         -> rule
            fillWidth -> rule `T.append` T.pack (replicate fillWidth fillPart)

-- | Render an ASCII horizontal rule
--
-- Note that the rendered horizontal rule maybe longer than the specified rule
-- width if the provided notes is too wide.
--
-- @since 0.3.0.0
renderAscii
  :: Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> Text
renderAscii = render asciiParts

-- | Render a Unicode horizontal rule
--
-- Note that the rendered horizontal rule maybe longer than the specified rule
-- width if the provided notes is too wide.
--
-- @since 0.3.0.0
renderUnicode
  :: Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> Text
renderUnicode = render unicodeParts

------------------------------------------------------------------------------

-- | Write a horizontal rule to the standard output device
--
-- @since 0.3.0.0
put
  :: Parts
  -> Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> IO ()
put parts width = TIO.putStrLn . render parts width

-- | Write an ASCII horizontal rule to the standard output device
--
-- @since 0.3.0.0
putAscii
  :: Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> IO ()
putAscii = put asciiParts

-- | Write a Unicode horizontal rule to the standard output device
--
-- @since 0.3.0.0
putUnicode
  :: Int     -- ^ rule width (characters)
  -> [Text]  -- ^ notes
  -> IO ()
putUnicode = put unicodeParts

------------------------------------------------------------------------------

-- | Write a full-width horizontal rule to the standard output device
--
-- The default rule width is used if the terminal width cannot be determined.
--
-- @since 0.3.0.0
putAuto
  :: Parts
  -> Int     -- ^ default rule width (characters)
  -> [Text]  -- ^ notes
  -> IO ()
putAuto parts defaultWidth notes = do
    width <- maybe defaultWidth Terminal.width <$> Terminal.size
    TIO.putStrLn $ render parts width notes

-- | Write a full-width ASCII horizontal rule to the standard output device
--
-- The default rule width is used if the terminal width cannot be determined.
--
-- @since 0.3.0.0
putAutoAscii
  :: Int     -- ^ default rule width (characters)
  -> [Text]  -- ^ notes
  -> IO ()
putAutoAscii = putAuto asciiParts

-- | Write a full-width Unicode horizontal rule to the standard output device
--
-- The default rule width is used if the terminal width cannot be determined.
--
-- @since 0.3.0.0
putAutoUnicode
  :: Int     -- ^ default rule width (characters)
  -> [Text]  -- ^ notes
  -> IO ()
putAutoUnicode = putAuto unicodeParts
