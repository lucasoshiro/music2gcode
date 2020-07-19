import System.Environment
import Data.List

import Song
import GCode

myPrinter = Printer
  { rangeX     = (10, 150)
  , rangeY     = (10, 150)
  , rangeZ     = (10, 100)
  , stepsPermm = (160 / 4, 160 / 4, 5120 / 4)
  }

main :: IO ()
main = do
  args <- getArgs
  let path = args !! 0
  content <- readFile path
  let ls = lines content
  let song = parseSong ls
  let (bpm, channels) = song
  let func = fromRelativeMovements myPrinter $ fromFreqEvents myPrinter $ fromSongEvents $ songEventsFromSong song

  putStrLn $ intercalate "\n" $ map show func

  -- let wave = pulsesFromSong song
  -- save wave "output.bin"
  
