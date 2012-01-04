module Data.MSM.Types (
    Surface(..),
    Air(..)
) where


data Surface = Surface {
    surfaceSeaLevelPressure  :: Float,
    surfacePressure          :: Float,
    surfaceWesterlyWind      :: Float,
    surfaceSoutherlyWind     :: Float,
    surfaceTemperature       :: Float,
    surfaceRelativeHumidity  :: Float,
    surfaceLowCloudAmount    :: Float,
    surfaceMiddleCloudAmount :: Float,
    surfaceHighCloudAmount   :: Float,
    surfaceTotalCloudAmount  :: Float,
    surfacePrecipitation     :: Float
} deriving Show


data Air = Air {
    airHeight           :: Float,
    airWesterlyWind     :: Float,
    airSoutherlyWind    :: Float,
    airTemperature      :: Float,
    airRelativeHumidity :: Float,
    airPVelocity        :: Float
} deriving Show


data Pressure = P1000
              | P975
              | P950
              | P925
              | P900
              | P850
              | P800
              | P700
              | P600
              | P500
              | P400
              | P300
              | P250
              | P200
              | P150
              | P100
 deriving Show
