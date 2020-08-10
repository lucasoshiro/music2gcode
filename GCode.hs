module GCode where
import Song
import Data.List
import Utils

type MM      = Float
type MM_s    = Float
type MiliSec = Int

type SongAction       = (MiliSec, Hz)          -- Duration, Frequency
type ChannelEvent     = (MiliSec, MiliSec, Hz) -- Begin, End, Frequency
type FreqEvent        = (MiliSec, Hz, Hz, Hz)  -- Begin, X freq, Y freq, Z freq
type Movement         = (MM, MM, MM, MM_s)     -- X Y Z F


data Printer = Printer { rangeX     :: (Float, Float)
                       , rangeY     :: (Float, Float)
                       , rangeZ     :: (Float, Float)
                       , stepsPermm :: (Float, Float, Float)
                       }

data Axis = X | Y | Z
data GCodeAtom = LinearMove {x :: Float, y :: Float, z :: Float, f :: Float}
               | Home

type GCode = [GCodeAtom]


instance Show GCodeAtom where
  show (LinearMove x y z f) = "G0 " ++ intercalate " " [
    k:(show v) | (k, v) <- zip "XYZF" [x, y, z, f]]

  show Home = "G28"


instance Show Axis where
  show X = "X"
  show Y = "Y"
  show Z = "Z"

homeSpeed :: MM_s
homeSpeed = 1000 -- Hardcoded. Fix it later.


fromSongAtom :: Int -> SongAtom -> SongAction
fromSongAtom bpm a = (p, f)
  where f = 2 * freq a
        t' (Silence s)      = s
        t' (Note (_, _, s)) = s
        t = t' a
        p = floor $ 1000 * period bpm t


fromChannel :: Int -> Channel -> [ChannelEvent]
fromChannel bpm channel = zip3 times (drop 1 times) freqs
  where actCh  = map (fromSongAtom bpm) channel
        deltas = map fst actCh
        freqs  = map snd actCh
        times  = foldl (\a b -> a ++ [last a + b]) [0] (deltas)


freqEventsFromSong :: Song -> [FreqEvent]
freqEventsFromSong (tempo, channels) = foldl update [(0, 0, 0, 0)] events
  where update l e = l ++ [(t, x, y, z)]
          where (_, old_x, old_y, old_z) = last l
                (t, _, ch, freq) = e
                x = if ch == 0 then freq else old_x
                y = if ch == 1 then freq else old_y
                z = if ch == 2 then freq else old_z

        events = sortBy timeorder $ concat songEvChannels
          where timeorder (a, _, _, _) (b, _, _, _) = compare a b

        evCh = map fromChannel' channels
          where fromChannel' = fromChannel tempo

        songEvChannels = zipWith fromChEvent [0..] evCh
          where fromChEvent i ch = map (\(b, e, f) -> (b, e, i, f)) ch


fromFreqEvents :: Printer -> [FreqEvent] -> [Movement]
fromFreqEvents printer events = clean
  where deltaTs = zipWith getDeltaT events $ drop 1 events
          where getDeltaT (t0, _, _, _) (t1, _, _, _) = fromIntegral (t1 - t0) / 1000

        steps = zipWith getFreqs deltaTs $ events
          where getFreqs dt (_, x, y, z) = (getFreq x, getFreq y, getFreq z)
                  where getFreq v = floor $ v * dt

        deltaSs = map fromSteps' steps
          where fromSteps' (x, y, z) = (fromSteps'' X x,
                                        fromSteps'' Y y,
                                        fromSteps'' Z z)
                fromSteps'' = fromSteps printer

        speeds = zipWith speed deltaSs deltaTs
          where speed (ds_x, ds_y, ds_z) dt = sqrt (ds_x ** 2 +
                                                    ds_y ** 2 +
                                                    ds_z ** 2) / dt * 60

        joined = zipWith joinF deltaSs speeds
          where joinF (x, y, z) f = (x, y, z, f)

        clean = filter hasMovement joined
          where hasMovement (x, y, z, _) = any (> 0) [x, y, z]


nextSafeMovements :: Printer -> Movement -> Movement -> [Movement]
nextSafeMovements printer (x, y, z, _) (dx, dy, dz, f) = 
  if and $ zipWith3 (\n nMin nMax -> nMin <= n && n <= nMax) target mins maxs
  then [mTarget]
  else mSafeTarget : nextSafeMovements printer mSafeTarget mDeltas

  where Printer (x0, xMax) (y0, yMax) (z0, zMax) _ = printer
        origin = [x,    y,    z]
        deltas = [dx,   dy,   dz]
        mins   = [x0,   y0,   z0]
        maxs   = [xMax, yMax, zMax]

        dirs = zipWith3 dir origin mins maxs
          where dir n n0 nMax = if (n - n0) < (nMax - n) then 1 else -1

        applyDelta o d s = o + s * d

        target = zipWith3 applyDelta origin deltas dirs
        mTarget = (target !! 0, target !! 1, target !! 2, f)

        (limArg, limDelta) = argMax deltas
        limDir = dirs !! limArg
        limDif =
          if limDir == 1
          then (origin !! limArg) - (mins !! limArg)
          else (maxs !! limArg) - (origin !! limArg)
        prop = limDif / limDelta

        safeDeltas = map (prop *) deltas
        restDeltas = zipWith (-) deltas safeDeltas
        safeTarget = zipWith3 applyDelta origin safeDeltas dirs

        mSafeTarget = (safeTarget !! 0, safeTarget !! 1, safeTarget !! 2, f)
        mDeltas     = (restDeltas !! 0, restDeltas !! 1, restDeltas !! 2, f)  


fromRelativeMovements :: Printer -> Bool -> [Movement] -> GCode
fromRelativeMovements printer homing movements = gcode
  where Printer (x0, xMax) (y0, yMax) (z0, zMax) _ = printer
        fstPos = (x0, y0, z0, homeSpeed) :: Movement
        nextSafeMovement' l d = l ++ nextSafeMovements printer (last l) d

        absolutes = foldl nextSafeMovement' [fstPos] movements
        gcode = preamble ++ map fromMovement absolutes
          where fromMovement (x, y, z, f) = LinearMove x y z f
                home = if homing then [Home] else []
                begin = [LinearMove x0 y0 z0 1000]
                preamble = home ++ begin

gCodeFromSong :: Printer -> Bool -> Song -> GCode
gCodeFromSong printer homing song = fromRelativeMovements printer homing
                                    $ fromFreqEvents printer
                                    $ freqEventsFromSong song


fromSteps :: Printer -> Axis -> Int -> MM
fromSteps (Printer _ _ _ (xmm, ymm, zmm)) = fromSteps'
  where fromSteps' X stp = fromIntegral stp / xmm
        fromSteps' Y stp = fromIntegral stp / ymm
        fromSteps' Z stp = fromIntegral stp / zmm


fromMM :: Printer -> Axis -> MM -> Int
fromMM (Printer _ _ _ (xmm, ymm, zmm)) = fromMM'
  where fromMM' X stp = floor $ stp * xmm
        fromMM' Y stp = floor $ stp * ymm
        fromMM' Z stp = floor $ stp * zmm
