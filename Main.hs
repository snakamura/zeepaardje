module Main (
    main
) where

import qualified Codec.Grib as Grib
import Control.Monad.Trans.Resource (runResourceT)
import Data.Binary.IEEE754 (wordToFloat)
import Data.Conduit (($$))
import Data.Conduit.Binary (sourceFile)
import Data.Conduit.Attoparsec (sinkParser)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  let path = head args
      n = read $ args !! 1
  grib <- runResourceT $ sourceFile path $$ sinkParser Grib.parser
  print $ length $ snd $ head $ Grib.sections grib
  let (_, s5, s7) = snd (head (Grib.sections grib)) !! n
  print $ take 10 $ drop 10000 $ Grib.decode s5 s7
  print $ take 10 $ drop 100000 $ Grib.decode s5 s7
  print $ wordToFloat $ Grib.r s5
  print $ Grib.d s5
  print $ Grib.e s5
