------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : hr: a horizontal rule for terminals
-- Copyright   : Copyright (c) 2019-2021 Travis Cardwell
-- License     : MIT
--
-- See the README for details.
------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- https://hackage.haskell.org/package/ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- https://hackage.haskell.org/package/base
import Control.Applicative (many)
import Data.Maybe (maybeToList)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T

-- https://hackage.haskell.org/package/time
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)

-- (hr:executable)
import qualified LibOA

-- (hr)
import qualified HR

------------------------------------------------------------------------------
-- $Constants

defaultFormat :: String
defaultFormat = "%Y-%m-%d %H:%M:%S"

defaultWidth :: Int
defaultWidth = 80

------------------------------------------------------------------------------
-- $Options

data Options
  = Options
    { optAscii  :: !Bool
    , optTime   :: !Bool
    , optFormat :: !String
    , optNote   :: !(Maybe String)
    }

options :: OA.Parser Options
options =
    Options
      <$> asciiOption
      <*> timeOption
      <*> formatOption
      <*> noteArguments

asciiOption :: OA.Parser Bool
asciiOption = OA.switch $ mconcat
    [ OA.long "ascii"
    , OA.short 'a'
    , OA.help "use ASCII lines"
    ]

timeOption :: OA.Parser Bool
timeOption = OA.switch $ mconcat
    [ OA.long "time"
    , OA.short 't'
    , OA.help "show time"
    ]

formatOption :: OA.Parser String
formatOption = OA.strOption $ mconcat
    [ OA.long "format"
    , OA.short 'f'
    , OA.metavar "FORMAT"
    , OA.value defaultFormat
    , OA.showDefaultWith id
    , OA.help "time format"
    ]

noteArguments :: OA.Parser (Maybe String)
noteArguments = fmap mjoin . many . OA.strArgument $ mconcat
    [ OA.metavar "NOTE ..."
    , OA.help "show a note"
    ]
  where
    mjoin :: [String] -> Maybe String
    mjoin [] = Nothing
    mjoin ss = Just $ unwords ss

------------------------------------------------------------------------------
-- $Main

main :: IO ()
main = do
    Options{..} <- OA.execParser pinfo
    mTime <- if optTime
      then Just . formatTime defaultTimeLocale optFormat <$> getZonedTime
      else pure Nothing
    let hr = if optAscii then HR.putAutoAscii else HR.putAutoUnicode
    hr defaultWidth $ T.pack <$> (maybeToList mTime ++ maybeToList optNote)
  where
    pinfo :: OA.ParserInfo Options
    pinfo
      = OA.info (LibOA.helper <*> LibOA.versioner HR.version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc "horizontal rule for the terminal"
          , OA.failureCode 2
          , OA.footerDoc $ Just formatHelp
          ]

    formatHelp :: Doc
    formatHelp = LibOA.section "FORMAT codes:" $ LibOA.table
      [ ("%Y", "four-digit year")
      , ("%y", "two-digit year")
      , ("%m", "two-digit month")
      , ("%d", "two-digit day")
      , ("%H", "two-digit hour using 24-hour clock")
      , ("%I", "two-digit hour using 12-hour clock")
      , ("%p", "locale equivalent of AM or PM")
      , ("%M", "two-digit minute")
      , ("%S", "two-digit second")
      , ("%f", "six-digit microsecond")
      , ("%z", "UTC offset")
      ]
