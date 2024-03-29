import System.Environment
import Data.Maybe

import Song
import Converter
import GuitarProTab

data Options = Options { printer    :: Maybe Printer
                       , inputPath  :: Maybe String
                       , outputPath :: Maybe String
                       , g28        :: Bool
                       , transpose  :: Int
                       }


defaultOpts :: Options
defaultOpts = Options { printer    = Nothing
                      , inputPath  = Nothing
                      , outputPath = Nothing
                      , g28        = False
                      , transpose  = 0
                      }


parseOptions :: [String] -> Options
parseOptions args = snd $ parseOptions' args defaultOpts


parseOptions' :: [String] -> Options -> ([String], Options)
parseOptions' [] opt = ([], opt)
parseOptions' args@(arg:_) opt = parseOptions' rest newOpt
  where (rest, newOpt) = case arg of
          "-p" -> parsePrinter args opt
          "-o" -> (drop 2 args, opt { outputPath = Just $ args !! 1 })
          "-h" -> (drop 1 args, opt { g28 = True })
          "-t" -> (drop 2 args, opt { transpose = read $ args !! 1})
          _    -> (drop 1 args, opt { inputPath  = Just $ args !! 0 })


parsePrinter :: [String] -> Options -> ([String], Options)
parsePrinter [] opt = ([], opt)
parsePrinter (_:args) opt = (rest, newOpt)
  where (argsP, rest) = splitAt 9 args
        ints :: [Float]
        ints = map read argsP
        newOpt = opt { printer = Just parsedPrinter }
        parsedPrinter = Printer
          { rangeX     = (ints !! 0, ints !! 1)
          , rangeY     = (ints !! 2, ints !! 3)
          , rangeZ     = (ints !! 4, ints !! 5)
          , stepsPermm = (ints !! 6, ints !! 7, ints !! 8)
          }


main :: IO ()
main = do
  args <- getArgs
  let (Options mPr mIn mOut homing tr) = parseOptions args

  let (printer', input, output) = fromJust $
        do printer'' <- mPr
           input'    <- mIn
           output'   <- mOut
           return (printer'', input', output')
                
  content <- readFile input
  let ls = lines content
  let song =
        if (drop (length input - 3) input) == "tab"
        then fromGuitarProTab ls
        else parseSong ls
  let trSong = if tr /= 0 then transposeSong song tr else song
  let gcode = gCodeFromSong printer' homing trSong
  writeFile output $ concat $ map ((++"\n") . show) gcode
