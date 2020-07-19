module GCode where
import Song
import Data.List


type MiliSec = Int

type SongAction   = (MiliSec, Hz)                     -- Duration, Frequency
type ChannelEvent = (MiliSec, MiliSec, Hz)            -- Begin, End, Frequency
type SongEvent    = (MiliSec, MiliSec, Int, Hz)       -- Begin, End, Channel, Frequency
type FreqEvent    = (MiliSec, Hz, Hz, Hz)             -- Begin, X freq, Y freq, Z freq
type PrinterEvent = (MiliSec, MiliSec, Int, Int, Int) -- Begin, End, Steps


data Printer = Printer { rangeX     :: (Float, Float)
                       , rangeY     :: (Float, Float)
                       , rangeZ     :: (Float, Float)
                       , stepsPermm :: (Float, Float, Float)}

data Axis = X | Y | Z
data GCodeAtom = LinearMove {x :: Float, y :: Float, z :: Float, f :: Float}
type GCode = [GCodeAtom]

data Dir = Backward | Forward
type Dir3D = (Dir, Dir, Dir)
type MM = Float
type MM_s = Float

type AxisEvent = (Dir, MM, MM_s)      -- Direction, End position, Speed
type RelativeMovement = (MM, MM, MM, MM_s)


instance Show GCodeAtom where
  show (LinearMove x y z f) = "G0 " ++ intercalate " " [
    k:(show v) | (k, v) <- zip "XYZF" [x, y, z, f]]


instance Show Axis where
  show X = "X"
  show Y = "Y"
  show Z = "Z"


instance Show Dir where
  show Backward = "<-"
  show Forward  = "<-"


fromDir :: Dir -> Int
fromDir Backward = -1
fromDir Forward  = 1


fromDir3D :: Dir3D -> (Int, Int, Int)
fromDir3D (a, b, c) = (fromDir a, fromDir b, fromDir c)


fromSongAtom :: Int -> SongAtom -> SongAction
fromSongAtom bpm a = (p, f)
  where f = 2 * freq a
        t' (Silence s)      = s
        t' (Noise (_, s))   = s
        t' (Note (_, _, s)) = s
        t = t' a
        p = floor $ 1000 * period bpm t


fromChannel :: Int -> Channel -> [ChannelEvent]
fromChannel bpm channel = zip3 times (drop 1 times) freqs
  where actCh  = map (fromSongAtom bpm) channel
        deltas = map fst actCh
        freqs  = map snd actCh
        times  = foldl (\a b -> a ++ [last a + b]) [0] (deltas)


songEventsFromSong :: Song -> [SongEvent]
songEventsFromSong (tempo, channels) = sortBy timeorder $ concat songEvChannels
  where fromChannel' = fromChannel tempo
        evCh = map fromChannel' channels
        fromChEvent i ch = map (\(b, e, f) -> (b, e, i, f)) ch
        songEvChannels = zipWith fromChEvent [0..] evCh
        timeorder (a, _, _, _) (b, _, _, _) = compare a b


fromSongEvents :: [SongEvent] -> [FreqEvent]
fromSongEvents events = foldl update [(0, 0, 0, 0)] events
  where update l e = l ++ [(t, x, y, z)]
          where (old_t, old_x, old_y, old_z) = last l
                (t, _, ch, freq) = e
                x = if ch == 0 then freq else old_x
                y = if ch == 1 then freq else old_y
                z = if ch == 2 then freq else old_z
        alpha = 10
          

fromFreqEvents :: Printer -> [FreqEvent] -> [RelativeMovement]
fromFreqEvents printer events = zipWith (\(x, y, z) f -> (x, y, z, f)) deltaSs speeds
  where deltaTs = zipWith getDeltaT events $ drop 1 events
        getDeltaT (t0, _, _, _) (t1, _, _, _) = fromIntegral (t1 - t0) / 1000
        steps = zipWith getFreqs deltaTs $ events
        getFreqs dt (t, x, y, z) = (floor $ x * dt, floor $ y * dt, floor $ z * dt)
        deltaSs = map (\(x, y, z) -> (fromSteps' X x, fromSteps' Y y, fromSteps' Z z)) steps
        fromSteps' = fromSteps printer
        speed (ds_x, ds_y, ds_z) dt = sqrt (ds_x ** 2 + ds_y ** 2 + ds_z ** 2) / dt * 60
        speeds = zipWith speed deltaSs deltaTs


fromRelativeMovements :: Printer -> [RelativeMovement] -> GCode
fromRelativeMovements printer movements = clean
  where toAbsolute l (dx, dy, dz, s) = l ++ [(x, y, z, s)]
          where (old_x, old_y, old_z, _) = last l
                x = if old_x - dx > x0 then old_x - dx else old_x + dx
                y = if old_y - dy > y0 then old_y - dy else old_y + dy
                z = if old_z - dz > z0 then old_z - dz else old_z + dz
        absolutes = foldl toAbsolute [(x0, y0, z0, 0)] movements
        Printer (x0, _) (y0, _) (z0, _) _ = printer
        fromMovement (x, y, z, f) = LinearMove x y z f
        gcode = map fromMovement absolutes
        clean = filter (\(LinearMove _ _ _ f) -> not $ isNaN f) gcode -- feio
                

fromSteps :: Printer -> Axis -> Int -> MM
fromSteps (Printer _ _ _ (xmm, ymm, zmm)) = fromSteps'
  where fromSteps' X stp = fromIntegral stp / xmm
        fromSteps' Y stp = fromIntegral stp / ymm
        fromSteps' Z stp = fromIntegral stp / zmm


fromMM :: Printer -> Axis -> MM -> Int
fromMM (Printer _ _ _ (xmm, ymm, zmm)) = fromMM'
  where fromMM' :: Axis -> MM -> Int
        fromMM' X stp = floor $ stp * xmm
        fromMM' Y stp = floor $ stp * ymm
        fromMM' Z stp = floor $ stp * zmm

