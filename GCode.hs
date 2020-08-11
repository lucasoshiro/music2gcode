module GCode where
import Data.List


data GCodeAtom = LinearMove {x :: Float, y :: Float, z :: Float, f :: Float}
               | Home

type GCode = [GCodeAtom]

instance Show GCodeAtom where
  show (LinearMove x y z f) = "G0 " ++ intercalate " " [
    k:(show v) | (k, v) <- zip "XYZF" [x, y, z, f]]

  show Home = "G28"
