module Data.MSM.Parser (
    parseSurface,
    topoParser,
    landSeaParser
) where

import qualified Codec.Grib as Grib
import Control.Applicative ((<$>))
import Data.Attoparsec.ByteString (Parser,
                                   many1)
import Data.Attoparsec.ByteString.Utils (anyFloat)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

import Data.MSM.Types


parseSurface :: Grib.Grib ->
                Surface
parseSurface grib = let [(section3, sections)] = Grib.sections grib
                    in Surface (parseMetadata $ Grib.section1 grib) $ parse 0 section3 sections IntMap.empty
    where
      parse n section3 sections map = let x = if n == 0 then 10 else 11
                                      in parse (n + 1) section3 (drop x sections) $ IntMap.insert n (parseSurfaceData section3 $ take x sections) map


parseMetadata :: Grib.Section1 ->
                 Metadata
parseMetadata section1 = Metadata (fromIntegral $ Grib.year section1)
                                  (fromIntegral $ Grib.month section1)
                                  (fromIntegral $ Grib.day section1)
                                  (fromIntegral $ Grib.hour section1)
                                  (fromIntegral $ Grib.minute section1)
                                  (fromIntegral $ Grib.second section1)

parseSurfaceData :: Grib.Section3 ->
                    [(Grib.Section4, Grib.Section5, Grib.Section7)] ->
                    Map Point SurfaceParameters
parseSurfaceData section3 sections = Map.fromList $ zip (points section3) parameters
    where
      parameters = zipListWith11 SurfaceParameters $ take 11 $ map parameter sections ++ [repeat (0.0 / 0.0)]
      parameter (_, section5, section7) = Grib.decode section5 section7


points :: Grib.Section3 ->
          [Point]
points section3 = [ Point (fromIntegral la) (fromIntegral lo) | la <- [Grib.la1 section3, Grib.la1 section3 - Grib.dj section3 .. Grib.la2 section3],
                                                                lo <- [Grib.lo1 section3, Grib.lo1 section3 + Grib.di section3 .. Grib.lo2 section3]]


zipListWith11 :: (a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> b) ->
                 [[a]] ->
                 [b]
zipListWith11 f parameters = let [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11] = map head parameters
                             in f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11:zipListWith11 f (map tail parameters)


topoParser :: Parser Topo
topoParser = Topo <$> floatMapParser


landSeaParser :: Parser LandSea
landSeaParser = LandSea <$> floatMapParser


floatMapParser :: Parser (Map Point Float)
floatMapParser = Map.fromList . zip topoPoints <$> many1 anyFloat


topoPoints :: [Point]
topoPoints = [ Point la lo | la <- [476000, 475500 .. 224000],
                             lo <- [1200000, 1200625 .. 1500000]]
