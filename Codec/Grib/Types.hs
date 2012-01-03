module Codec.Grib.Types (
    Grib(..)
) where

import Data.Word (Word8,
                  Word64)


data Grib = Grib {
    version :: Word8,
    length  :: Word64
} deriving Show


