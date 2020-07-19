module Song where

import Data.Char
import Data.List

type Hz = Float
type Sec = Float

data SongAtom = Silence Sec | Noise (Int, Sec) | Note (String, Int, Sec)
type Channel = [SongAtom]
type Song    = (Int, [Channel])


c0 :: Float
c0 = 16.351597831287418

baseExp :: Hz
baseExp = 1.0594630943592953


fromFigure :: String -> Int
fromFigure s =
  let conv = [0, 2, 4, 5, 7, 9, 11]
      base = conv !! (((ord $ s !! 0) - (ord 'A') + 12) `mod` 7)
      ac = if (length s) == 1 then 0
           else if (s !! 1) == '#' then 1 else -1
  in base + ac


period :: Int -> Float -> Sec
period bpm beats = 60 * beats / (fromIntegral bpm)


semitones :: String -> Int -> Int
semitones figure octave = octave * 12 + fromFigure figure


freq :: SongAtom -> Hz
freq (Note n) =
  let (figure, octave, _) = n
      mult = fromIntegral $ (2 ^ octave :: Int) :: Float
  in mult * c0 * baseExp ** (fromIntegral $ fromFigure figure)
freq (Silence _) = 0.0
freq (Noise _) = 0.0


parseSongAtom :: String -> SongAtom
parseSongAtom s =
  let first:params = words s
  in case first of
    "-" -> Silence (read $ params !! 0)
    "~" -> Noise   (read $ params !! 0, read $ params !! 1)
    _   -> Note    (first, read $ params !! 0, read $ params !! 1)


parseChannel :: [String] -> Channel
parseChannel = map parseSongAtom


preProcessSong :: [String] -> [String]
preProcessSong = clear . (map $ uncomment . unwspace)
  where uncomment   = takeWhile (/= '%')
        unwspace    = dropWhile (== ' ')
        clear       = filter allow :: [String] -> [String]
        nonempty x  = length x > 0
        notChannels = not . isPrefixOf "CHANNELS"
        allow x     = (notChannels x) && (nonempty x)


parseSong' :: [String] -> Song
parseSong' [] = (0, [])
parseSong' (l:ls) =
  if "TEMPO" `isPrefixOf` l
  then
    (read $ drop 6 l, parseSongChannels ls)
  else parseSong' ls


parseSongChannels :: [String] -> [Channel]
parseSongChannels [] = []
parseSongChannels (l:ls) =
  if "BEGINCH" `isPrefixOf` l
  then
    let (rawChannel, etc) = break ("ENDCH" `isPrefixOf`) ls
    in (parseChannel rawChannel):(parseSongChannels $ drop 1 etc)
    else []


parseSong :: [String] -> Song
parseSong = parseSong' . preProcessSong


unparseSongAtom :: SongAtom -> String
unparseSongAtom (Silence t) = "- " ++ show t
unparseSongAtom (Noise (o, t)) = "~ " ++ show o ++ " " ++ show t
unparseSongAtom (Note (f, o, t)) = f ++ " " ++ show o ++ " " ++ show t
  

unparseChannel :: Channel -> [String]
unparseChannel c = ["BEGINCH"] ++ (map unparseSongAtom c) ++ ["ENDCH"]


unparseSong :: Song -> [String]
unparseSong (tempo, channels) =
  [ "TEMPO " ++ show tempo
  , "CHANNELS " ++ (show $ length channels)] ++
  [line | chLines <- map unparseChannel channels, line <- chLines]