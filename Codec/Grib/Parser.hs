module Codec.Grib.Parser (
    parser
) where

import Control.Applicative ((<$>),
                            (<*>),
                            (*>),
                            (<*))
import Control.Monad (void)
import Data.Attoparsec.ByteString (Parser,
                                   anyWord8,
                                   endOfInput,
                                   string,
                                   take,
                                   word8)
import Data.Attoparsec.ByteString.Utils (anyInt16,
                                         anyWord16,
                                         anyWord32,
                                         anyWord64,
                                         word16,
                                         word32)
import Data.Attoparsec.Combinator (many1)
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
