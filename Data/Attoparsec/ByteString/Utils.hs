module Data.Attoparsec.ByteString.Utils (
    word16,
    word32,
    word64,
    anyWord16,
    anyWord32,
    anyWord64,
    anyInt16,
    anyFloat
) where

import Control.Applicative ((<$>),
                            (*>),
                            pure)
import Data.Attoparsec.ByteString (Parser,
                                   anyWord8,
                                   string)
import Data.Binary.IEEE754 (wordToFloat)
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


anyFloat :: Parser Float
anyFloat = wordToFloat <$> anyWord32


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
