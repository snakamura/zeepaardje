module Data.MSM.Types (
    MSM(..),
    Metadata(..),
    Point(..),
    Surface(..),
    Air(..),
    Pressure(..)
) where

import Data.IntMap (IntMap)
import Data.Map (Map)


data MSM = MSMSurface Metadata (IntMap (Map Point Surface))
         | MSMAir Metadata (Map Pressure (IntMap (Map Point Air)))
  deriving Show


data Metadata = Metadata {
    year   :: Int,
    month  :: Int,
    day    :: Int,
    hour   :: Int,
    minute :: Int,
    second :: Int
} deriving Show


data Point = Point {
    latitude  :: Int,
    longitude :: Int
} deriving Show


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
