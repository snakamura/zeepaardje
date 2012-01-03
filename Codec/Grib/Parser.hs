module Codec.Grib.Parser (
    parser
) where

import Control.Applicative ((<$>),
                            (<*>),
                            (*>))
import Data.Attoparsec.ByteString (Parser,
                                   anyWord8,
                                   string,
                                   word8)
import Data.Bits ((.|.),
                  shift)
import Data.List (foldl')
import Data.Word (Word64)

import Codec.Grib.Types (Grib(..))


parser :: Parser Grib
parser = header *> (Grib <$> anyWord8 <*> anyWord64)
    where
      header = string "GRIB" *>
               word8 0xff *>
               word8 0xff *>
               word8 0


anyWord64 :: Parser Word64
anyWord64 = foldl' (.|.) 0 <$> sequence (map anyWord8WithShift [7, 6..0])
    where
      anyWord8WithShift n = flip shift (n*8) . fromIntegral <$> anyWord8
