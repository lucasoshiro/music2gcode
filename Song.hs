module Song where

import Data.Char
import Data.List

type Hz = Float
type Sec = Float
type Bpm = Int

data SongAtom = Silence Sec | Note (String, Int, Sec)
type Channel = [SongAtom]
type Song    = (Bpm, [Channel])


instance Show SongAtom where
  show (Silence t)      = intercalate " " ["-", show t]
  show (Note (f, o, t)) = intercalate " " [f,   show o, show t]


c0 :: Float
c0 = 16.351597831287418

baseExp :: Hz
baseExp = 1.0594630943592953


fromFigure :: String -> Int
fromFigure s =  base + ac
  where conv = [0, 2, 4, 5, 7, 9, 11]
        base = conv !! (((ord $ s !! 0) - (ord 'A') + 12) `mod` 7)
        ac = if (length s) == 1 then 0
             else if (s !! 1) == '#' then 1 else -1


period :: Int -> Float -> Sec
period bpm beats = 60 * beats / (fromIntegral bpm)


freq :: SongAtom -> Hz
freq (Silence _) = 0.0
freq (Note n) = mult * c0 * baseExp ** (fromIntegral $ fromFigure figure)
  where (figure, octave, _) = n
        mult = fromIntegral $ ((2 :: Int) ^ octave)


parseSongAtom :: String -> SongAtom
parseSongAtom s = case first of
  "-" -> Silence (read $ params !! 0)
  _   -> Note    (first, read $ params !! 0, read $ params !! 1)
  where first:params = words s


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
parseSong' (l:ls) = if "TEMPO" `isPrefixOf` l
                    then (read $ drop 6 l, parseSongChannels ls)
                    else parseSong' ls


parseSongChannels :: [String] -> [Channel]
parseSongChannels [] = []
parseSongChannels (l:ls) =
  if "BEGINCH" `isPrefixOf` l
  then (parseChannel rawChannel):(parseSongChannels $ drop 1 etc)
  else []
  where (rawChannel, etc) = break ("ENDCH" `isPrefixOf`) ls


parseSong :: [String] -> Song
parseSong = parseSong' . preProcessSong


unparseChannel :: Channel -> [String]
unparseChannel c = ["BEGINCH"] ++ (map show c) ++ ["ENDCH"]


unparseSong :: Song -> [String]
unparseSong (tempo, channels) =
  [ "TEMPO "    ++ show tempo
  , "CHANNELS " ++ (show $ length channels)] ++
  [line | chLines <- map unparseChannel channels, line <- chLines]


transposeSong :: Song -> Int -> Song
transposeSong (b, channels) octaves = (b, trChannels)
  where trChannels = map transposeCh channels
        transposeCh ch = map transposeAtom ch

        transposeAtom (Note (s, o, t)) = Note (s, o + octaves, t)
        transposeAtom a = a
