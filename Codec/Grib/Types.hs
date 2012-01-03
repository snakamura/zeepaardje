module Codec.Grib.Types (
    Grib(..),
    Section0(..),
    Section1(..)
) where

import Data.Word (Word16,
                  Word64,
                  Word8)


data Grib = Grib {
    section0 :: Section0,
    section1 :: Section1
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
