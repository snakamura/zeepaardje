module Codec.Grib.Parser (
    parser
) where

import Control.Applicative ((<$>),
                            (<*>),
                            (*>),
                            (<*),
                            pure)
import Control.Monad (void)
import Data.Attoparsec.ByteString (Parser,
                                   anyWord8,
                                   endOfInput,
                                   string,
                                   take,
                                   word8)
import Data.Attoparsec.Combinator (many1)
import Data.Bits (Bits,
                  (.&.),
                  (.|.),
                  bitSize,
                  shift,
                  testBit)
import qualified Data.ByteString as B
import Data.Int (Int16)
import Data.List (foldl',
                  mapAccumL)
import Data.Word (Word16,
                  Word32,
                  Word64,
                  Word8)
import Prelude hiding (take)

import Codec.Grib.Types (Grib(Grib),
                         Section0(Section0),
                         Section1(Section1),
                         Section3(Section3),
                         Section4(Section4),
                         Section5(Section5),
                         Section7(Section7))


parser :: Parser Grib
parser = Grib <$> section0 <*> section1 <*> many1 sections <* section8 <* endOfInput


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
           (Section1 <$> anyWord16
                     <*> anyWord8
                     <*> anyWord8
                     <*> anyWord8
                     <*> anyWord8
                     <*> anyWord8) <*
           word8 0 <*
           word8 1


sections :: Parser (Section3, [(Section4, Section5, Section7)])
sections = (,) <$> section3 <*> many1 sections2

sections2 :: Parser (Section4, Section5, Section7)
sections2 = (,,) <$> section4 <*> section5 <* section6 <*> section7


section3 :: Parser Section3
section3 = word32 72 *>
           word8 3 *>
           word8 0 *>
           anyWord32 *>
           word8 0 *>
           word8 0 *>
           word16 0 *>
           word8 6 *>
           word8 0xff *>
           word32 0xffffffff *>
           word8 0xff *>
           word32 0xffffffff *>
           word8 0xff *>
           word32 0xffffffff *>
           (Section3 <$> anyWord32
                     <*> anyWord32 <* anyWord32 <* anyWord32
                     <*> anyWord32
                     <*> anyWord32 <* anyWord8
                     <*> anyWord32
                     <*> anyWord32
                     <*> anyWord32
                     <*> anyWord32) <*
           anyWord8


section4 :: Parser Section4
section4 = anyWord32 >>= \len ->
           word8 4 *>
           word16 0 *>
           anyWord16 *>
           (Section4 <$> anyWord8
                     <*> anyWord8 <* anyWord8 <* word8 0x1f <* word8 0xff <* anyWord16 <* anyWord8 <* anyWord8
                     <*> anyWord32
                     <*> anyWord8 <* anyWord8
                     <*> anyWord32) <*
           take (fromIntegral len - 28)


section5 :: Parser Section5
section5 = word32 21 *>
           word8 5 *>
           (Section5 <$> anyWord32 <* word16 0
                     <*> anyWord32
                     <*> anyInt16
                     <*> anyInt16
                     <*> anyWord8) <*
           word8 0


section6 :: Parser ()
section6 = void $ word32 6 *>
                  word8 6 *>
                  word8 0xff


section7 :: Parser Section7
section7 = anyWord32 >>= \len ->
           word8 7 *>
           (Section7 <$> take (fromIntegral len - 5))


section8 :: Parser ()
section8 = void $ string "7777"


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


anyInt16 :: Parser Int16
anyInt16 = conv <$> anyWord16
    where
      conv n | testBit n 15 = negate $ fromIntegral $ n .&. 0x7fff
             | otherwise    = fromIntegral n


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
