module Data.MSM.Types (
    Surface(..),
    Air(..),
    Metadata(..),
    Point(..),
    SurfaceParameters(..),
    AirParameters(..),
    Pressure(..),
    Topo(..),
    LandSea(..)
) where

import Data.IntMap (IntMap)
import Data.Map (Map)


data Surface = Surface Metadata (IntMap (Map Point SurfaceParameters))
  deriving Show

data Air = Air Metadata (Map Pressure (IntMap (Map Point AirParameters)))
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
} deriving (Show, Eq, Ord)


data SurfaceParameters = SurfaceParameters {
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


data AirParameters = AirParameters {
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


data Topo = Topo (Map Point Float)
  deriving Show


data LandSea = LandSea (Map Point Float)
  deriving Show
