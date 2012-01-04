module Codec.Grib.Types (
    Grib(..),
    Section0(..),
    Section1(..),
    Section3(..),
    Section4(..),
    Section5(..),
    Section7(..),
    decode
) where

import Data.Binary.IEEE754 (wordToFloat)
import Data.Bits ((.&.),
                  (.|.),
                  shift)
import qualified Data.ByteString as B
import Data.Int (Int16)
import Data.Word (Word16,
                  Word32,
                  Word64,
                  Word8)


data Grib = Grib {
    section0 :: Section0,
    section1 :: Section1,
    sections :: [(Section3, [(Section4, Section5, Section7)])]
} deriving Show


data Section0 = Section0 {
    version :: Word8,
    length  :: Word64
} deriving Show


data Section1 = Section1 {
    year   :: Word16,
    month  :: Word8,
    day    :: Word8,
    hour   :: Word8,
    minute :: Word8,
    second :: Word8
} deriving Show


data Section3 = Section3 {
    ni  :: Word32,
    nj  :: Word32,
    la1 :: Word32,
    lo1 :: Word32,
    la2 :: Word32,
    lo2 :: Word32,
    di  :: Word32,
    dj  :: Word32
} deriving Show


data Section4 = Section4 {
    parameterType   :: Word8,
    parameterNumber :: Word8,
    layer1Type      :: Word8,
    layer1Number    :: Word32
} deriving Show


data Section5 = Section5 {
    num :: Word32,
    r   :: Word32,
    e   :: Int16,
    d   :: Int16,
    b   :: Word8
} deriving Show


data Section7 = Section7 {
    x :: B.ByteString
} deriving Show


decode :: Section5 ->
          Section7 ->
          [Float]
decode section5 section7 = go (x section7)
    where
      go s | B.null s  = []
           | otherwise = let (c, cs) = B.splitAt 3 s
                         in decode2 (B.unpack c) ++ go cs
      decode2 [b1, b2, b3] = let w1 = shift (shift (fromIntegral b1) 8 .|. fromIntegral b2) (-4) .&. 0x0fff
                                 w2 = (shift (fromIntegral b2) 8 .|. fromIntegral b3) .&. 0x0fff
                             in map (decodeWord section5) [w1, w2]
      decode2 [b1, b2] = take 1 $ decode2 [b1, b2, 0]
      decode2 _ = error "Never occurs."


decodeWord :: Section5 ->
              Word16 ->
              Float
decodeWord section5 v = (wordToFloat (r section5) + fromIntegral v * (2.0 ^^ e section5)) / (10.0 ^^ d section5)
