------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : hr: a horizontal rule for terminals
-- Copyright   : Copyright (c) 2019-2021 Travis Cardwell
-- License     : MIT
--
-- See the README for details.
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- https://hackage.haskell.org/package/ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- https://hackage.haskell.org/package/base
import Control.Applicative (many)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.Version (showVersion)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- https://hackage.haskell.org/package/terminal-size
import qualified System.Console.Terminal.Size as Terminal

-- https://hackage.haskell.org/package/time
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)

-- (hr:cabal)
import qualified Paths_hr as Project

-- (hr:executable)
import qualified LibOA

------------------------------------------------------------------------------

-- | Default time format
defaultFormat :: String
defaultFormat = "%Y-%m-%d %H:%M:%S"

-- | Default terminal width
defaultWidth :: Int
defaultWidth = 80

------------------------------------------------------------------------------

-- | Render a horizontal rule
hr :: Bool -> Int -> [String] -> String
hr ascii width = \case
    [] -> replicate width rule_fill
    notes ->
      let rule = rule_left ++ intercalate rule_mid notes ++ rule_right
          rule_len = length rule
      in  if rule_len < width
            then rule ++ replicate (width - rule_len) rule_fill
            else rule
  where
    rule_left = if ascii then "--|" else "━━┫"
    rule_mid = if ascii then "|-|" else "┣━┫"
    rule_right = if ascii then "|--" else "┣━━"
    rule_fill = if ascii then '-' else '━'

------------------------------------------------------------------------------

-- | Command-line options
data Options
  = Options
    { optAscii  :: !Bool
    , optTime   :: !Bool
    , optFormat :: !String
    , optNote   :: !(Maybe String)
    }
  deriving Show

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

main :: IO ()
main = do
    Options{..} <- OA.execParser pinfo
    mTime <- if optTime
      then Just . formatTime defaultTimeLocale optFormat <$> getZonedTime
      else pure Nothing
    width <- maybe defaultWidth Terminal.width <$> Terminal.size
    putStrLn . hr optAscii width $ maybeToList mTime ++ maybeToList optNote
  where
    pinfo :: OA.ParserInfo Options
    pinfo
      = OA.info (LibOA.helper <*> LibOA.versioner version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc "horizontal rule for the terminal"
          , OA.failureCode 2
          , OA.footerDoc $ Just formatHelp
          ]

    version :: String
    version = "hr-haskell " ++ showVersion Project.version

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
