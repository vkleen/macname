{-# language BangPatterns #-}
module Main where

import           Control.Category ((>>>))
import           Control.Monad (when)
import           Crypto.Hash
import           Crypto.Random (MonadRandom(..))
import           Data.Bits
import           Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Lazy as BL
import           Data.Text ()
import           Data.Text.Encoding (decodeUtf8)
import           Options.Applicative
import qualified Radium.Element as C
import           System.IO (hFlush, stdout)
import           Text.Printf (printf)

byteStringToElement :: BS.ByteString -> C.Element
byteStringToElement =     hashWith Blake2b_512
                      >>> convert
                      >>> BS.foldl1' xor
                      >>> fromEnum
                      >>> (`mod` 118)
                      >>> (+ 1)
                      >>> C.element

randomBytes :: MonadRandom m => m BS.ByteString
randomBytes = getRandomBytes 16

render :: BS.ByteString -> BS.ByteString
render = BL.byteStringHex >>> BL.toLazyByteString >>> BL.toStrict

output :: Int -> BS.ByteString -> Int -> Int -> IO ()
output n bs t1 t2 = do
  printf "\r\x1b[K%10d %s (%s): %3d %d"
    n
    (decodeUtf8 $ render bs)
    (decodeUtf8.render $ BS.take 4 bs)
    t1 t2
  hFlush stdout

tryOnce :: IO (BS.ByteString, C.Element, C.Element)
tryOnce = do
  bs <- randomBytes
  let t1 = byteStringToElement (render bs)
      t2 = byteStringToElement (render $ BS.take 4 bs)
  pure (bs, t1, t2)

bruteforce :: C.Element -> IO BS.ByteString
bruteforce e = go 0
  where
    go :: Int -> IO BS.ByteString
    go !n = do
      (bs, t1, t2) <- tryOnce
      when (n `mod` 256 == 0) $ output n bs (C.atomicNumber t1) (C.atomicNumber t2)
      if (e == t1) && (e == t2)
        then output n bs (C.atomicNumber t1) (C.atomicNumber t2) >> pure bs
        else go (n+1)

data Mode = Hash BS.ByteString | Bruteforce Int

run :: Mode -> IO ()
run (Hash a) = let el = byteStringToElement a
               in printf "%s = %3d %s\n" (decodeUtf8 a) (C.atomicNumber el) (C.name el)

run (Bruteforce e) = do
  bs <- bruteforce (C.element e)
  printf "\n"
  run . Hash . render $ bs

hashOptions :: Parser Mode
hashOptions = Hash <$>
  argument str (metavar "<data>")

searchOptions :: Parser Mode
searchOptions = Bruteforce <$>
   argument auto (metavar "<atomic number>")

opts :: Parser Mode
opts =
  hsubparser
    (  command "hash" (info hashOptions
         (progDesc "Hash a string into an element using Blake2b_512."))
    <> command "search" (info searchOptions
         (progDesc "Find a hexadecimal string of length 32 such that it and its 8 character prefix hash to a given element."))
    )

main :: IO ()
main = customExecParser (prefs showHelpOnError) o >>= run
  where o = info (helper <*> opts) (  fullDesc
                                   <> progDesc "Generate hostnames from random data.")
