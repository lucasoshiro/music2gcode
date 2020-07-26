import System.Environment
import Data.Maybe

import Song
import GCode

data Options = Options { printer    :: Maybe Printer
                       , inputPath  :: Maybe String
                       , outputPath :: Maybe String
                       , g28        :: Bool
                       }


defaultOpts :: Options
defaultOpts = Options { printer    = Nothing
                      , inputPath  = Nothing
                      , outputPath = Nothing
                      , g28        = False
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
  let (Options mPr mIn mOut homing) = parseOptions args

  let (printer', input, output) = fromJust $
        do printer'' <- mPr
           input'    <- mIn
           output'   <- mOut
           return (printer'', input', output')
                
  content <- readFile input
  let ls = lines content
  let song = parseSong ls
  let gcode = gCodeFromSong printer' homing song
  writeFile output $ concat $ map ((++"\n") . show) gcode
