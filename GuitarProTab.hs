module GuitarProTab where

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Song

fromGuitarProTab :: [String] -> Song
fromGuitarProTab tabLines = (100, [[], [], ch])
  where ch = channelFromTab . joinSystems . getSystems $ tabLines

isStringLine :: String -> Bool
isStringLine line =
  length line > 0 &&
  hasMeasureDivision &&
  (hasDash || hasStringName && hasHeader)

  where
    firstChar          = head line
    hasMeasureDivision = last line == '|'
    hasDash            = firstChar == '-'
    hasStringName      = ('A' <= firstChar) && (firstChar <= 'G')
    hasHeader          = length line > 3 && ((take 2) . (drop 1) $ line) == "||"

getSystems :: [String] -> [[String]]
getSystems l = reverse . map reverse . filter (/= [])$ systems
  where stringLines = map isStringLine l
        keep = zipWith (||) stringLines (tail stringLines)
        classified = zip keep l

        appendToSystems [] _ = [[]]
        appendToSystems (f:ss) (k, s) =
          if k
          then (s:f) : ss
          else [] : f : ss
        systems = foldl appendToSystems [] classified

joinSystems :: [[String]] -> [String]
joinSystems [] = []
joinSystems systems = map concat transposed
  where
    transposed = transpose systems

printSystems :: [[String]] -> IO ()
printSystems systems = do
  let printable = intercalate "\n\n" . map (intercalate "\n") $ systems
  putStrLn printable

printTab :: [String] -> IO ()
printTab tab = printSystems [tab]

tr :: (String, Int) -> Int -> (String, Int)
(base, oct) `tr` s = (notes !! new_note, new_octave)
  where
    notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
    base_note = fromJust . findIndex (== base) $ notes
    new_note = (base_note + s) `mod` 12
    new_octave = (base_note + s) `div` 12 + oct

sub :: (String, Int) -> (String, Int) -> Int
(na, oa) `sub` (nb, ob) = a - b
  where
    notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
    a = oa * 12 + (fromJust . findIndex (== na) $ notes)
    b = ob * 12 + (fromJust . findIndex (== nb) $ notes)

channelFromTab :: [String] -> Channel
channelFromTab lines = reverse $ foldl appendColumn [] transposed
  where
    bpm = 100 --hardcoded

    timeFigures = M.fromList $
      [ ('W', 4.0)
      , ('H', 2.0)
      , ('Q', 1.0)
      , ('E', 1.0/2)
      , ('S', 1.0/4)
      , ('T', 1.0/8)
      ]

    standardTuning =
      [ ("E", 3)
      , ("A", 3)
      , ("D", 4)
      , ("G", 4)
      , ("B", 4)
      , ("E", 5)
      ]
      
    transposed = transpose lines

    appendColumn :: Channel -> String -> Channel
    appendColumn ch [] = ch
    appendColumn ch [_] = ch
    appendColumn ch (' ':_) = ch
    appendColumn [] (d:strs) = [newAtom]
      where
        isSilence = all (== '-') strs
        duration = (60 / bpm) * (timeFigures M.! d)
        

        curString = fromJust . findIndex (/= '-') $ strs
        base = (reverse standardTuning) !! curString
        fret = read [strs !! curString] :: Int
        (n, oct) = base `tr` fret

        newAtom =
          if isSilence
          then Silence duration
          else Note (n, oct, duration)

      
    appendColumn (lst:ch) (d:strs) =
      if isLigature || isDot
      then replaceLig : ch
      else if isFullNote
           then newAtom : lst : ch
           else replaceDec : ch

      where
        isSilence = all (== '-') strs
        isLigature = any (== 'L') strs
        isDot = d == '.'
        isFullNote = d /= ' '
        
        lastDuration = case lst of
          Note (_, _, s) -> s
          Silence s      -> s
          
        duration =
          if isDot
          then lastDuration * 3 / 2
          else
            if isLigature
            then lastDuration + (60 / bpm) * (timeFigures M.! d)
            else (60 / bpm) * (timeFigures M.! d)
        
        curString = fromJust . findIndex (/= '-') $ strs
        base = (reverse standardTuning) !! curString
        fret = read [strs !! curString] :: Int
        (n, oct) = base `tr` fret
  
        replaceLig = case lst of
          Note (n', oct', _) -> Note (n', oct', duration)
          Silence _          -> Silence duration

        replaceDec = case lst of
          Note (n', oct', _) -> Note (n'', oct'', duration)
            where
              dif = (n', oct') `sub` base
              fret' = dif * 10 + fret
              (n'', oct'') = base `tr` fret'
          _ -> lst

        newAtom =
          if isSilence
          then Silence duration
          else Note (n, oct, duration)
