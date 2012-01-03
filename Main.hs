module Main (
    main
) where

import qualified Codec.Grib as Grib
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$))
import Data.Conduit.Binary (sourceFile)
import Data.Conduit.Attoparsec (sinkParser)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  let path = head args
  grib <- runResourceT $ sourceFile path $$ sinkParser Grib.parser
  print grib
