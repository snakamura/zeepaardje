module Codec.Grib.Parser (
    parser
) where

import Control.Applicative ((<$>),
                            (<*>),
                            (*>),
                            (<*),
                            pure)
import Data.Attoparsec.ByteString (Parser,
                                   anyWord8,
                                   string,
                                   word8)
import Data.Bits (Bits,
                  (.&.),
                  (.|.),
                  bitSize,
                  shift)
import qualified Data.ByteString as B
import Data.List (foldl',
                  mapAccumL)
import Data.Word (Word16,
                  Word32,
                  Word64,
                  Word8)

import Codec.Grib.Types (Grib(Grib),
                         Section0(Section0),
                         Section1(Section1))


parser :: Parser Grib
parser = Grib <$> section0 <*> section1


section0 :: Parser Section0
section0 = header *> (Section0 <$> anyWord8 <*> anyWord64)
    where
      header = string "GRIB" *>
               word16 0xffff *>
               word8 0


section1 :: Parser Section1
section1 = word32 21 *>
           word8 1 *>
           word16 34 *>
           word16 0 *>
           word8 2 *>
           word8 1 *>
           word8 1 *>
           (Section1 <$> anyWord16 <*> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8) <*
           word8 0 <*
           word8 1


word16 :: Word16 ->
          Parser Word16
word16 = word


word32 :: Word32 ->
          Parser Word32
word32 = word


word64 :: Word64 ->
          Parser Word64
word64 = word



anyWord16 :: Parser Word16
anyWord16 = anyWord 2


anyWord32 :: Parser Word32
anyWord32 = anyWord 4


anyWord64 :: Parser Word64
anyWord64 = anyWord 8


word :: (Bits b, Integral b) =>
        b ->
        Parser b
word n = string (B.pack $ bitsToWord8s n) *> pure n


anyWord :: Bits b =>
           Int ->
           Parser b
anyWord n = word8sToBits <$> sequence (replicate n anyWord8)


word8sToBits :: Bits b =>
                [Word8] ->
                b
word8sToBits = foldl' f 0
    where
      f p n = shift p 8 .|. fromIntegral n


bitsToWord8s :: (Bits b, Integral b) =>
                b ->
                [Word8]
bitsToWord8s n = reverse $ snd $ mapAccumL f n [1 .. bitSize n `div` 8]
    where
      f m _ = (shift m (-8), fromIntegral $ m .&. 0xff)
