module Main where

import           Control.Category            ((>>>))
import           Control.Monad               (forM_)
import qualified Crypto.Hash.SHA256          as SHA256
import           Data.Bits
import qualified Data.ByteString             as BS
import qualified Radium.Element              as C
import           System.Posix.Env.ByteString (getArgs)
import           Text.Printf                 (printf)

bytestringToElement :: BS.ByteString -> C.Element
bytestringToElement =     SHA256.hash
                      >>> BS.foldl1' xor
                      >>> fromEnum
                      >>> (`mod` 118)
                      >>> (+ 1)
                      >>> C.element

main :: IO ()
main = do
    as <- getArgs
    forM_ as $ \a -> do
      let el = bytestringToElement a
      BS.putStr a
      printf ": %3d = %s\n" (C.atomicNumber el) (C.name el)
