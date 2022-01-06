{-# language BangPatterns, NamedFieldPuns, RecordWildCards, ApplicativeDo #-}
module Main where

import           Control.Category ((>>>))
import           Control.Monad (when, unless)
import           Crypto.Hash
import           Data.Bits
import           Data.Word (Word64)
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
import           Data.Function ((&))
import           System.Exit (exitFailure)

byteStringToElement :: BS.ByteString -> C.Element
byteStringToElement =     hashWith Blake2b_512
                      >>> convert
                      >>> BS.foldl1' xor
                      >>> fromEnum
                      >>> (`mod` 118)
                      >>> (+ 1)
                      >>> C.element

render :: BS.ByteString -> BS.ByteString
render = BL.byteStringHex >>> BL.toLazyByteString >>> BL.toStrict

output :: Word64 -> BS.ByteString -> Int -> Int -> Int -> IO ()
output n bs t1 t2 t3 = do
  printf "\r\x1b[K%10d %s (%s) (%s): %3d %d %d"
    n
    (decodeUtf8 $ render bs)
    (decodeUtf8.render $ BS.take 4 bs)
    (decodeUtf8.render $ BS.take 2 bs)
    t1 t2 t3
  hFlush stdout

tryOnce :: BS.ByteString -> Word64 -> (BS.ByteString, C.Element, C.Element, C.Element)
tryOnce ns n =
  let bs = BS.take 16 . convert . hashWith Blake2b_512 .
           BL.toStrict . BL.toLazyByteString $ BL.byteString ns <> BL.word64HexFixed n
      t1 = byteStringToElement (render bs)
      t2 = byteStringToElement (render $ BS.take 4 bs)
      t3 = byteStringToElement (render $ BS.take 2 bs)
  in (bs, t1, t2, t3)

bruteforce :: Bool -> BS.ByteString -> C.Element -> IO BS.ByteString
bruteforce quiet ns e = go 0
  where
    go :: Word64 -> IO BS.ByteString
    go !n = do
      let (bs, t1, t2, t3) = tryOnce ns n
      unless quiet $ when (n `mod` 0x100 == 0 && n `mod` 0xff == 0) $
        output n bs (C.atomicNumber t1) (C.atomicNumber t2) (C.atomicNumber t3)
      if (e == t1) && (e == t2) && (e == t3)
        then do
          unless quiet $ output n bs (C.atomicNumber t1) (C.atomicNumber t2) (C.atomicNumber t3)
          pure bs
        else go (n+1)

data Mode = Hash BS.ByteString | Bruteforce BS.ByteString String

data Args = Args { quiet :: Bool
                 , mode :: Mode
                 }

run :: Args -> IO ()
run Args { quiet, mode = Hash a } =
  let el = byteStringToElement a
  in case quiet of
    True -> printf "%d %s\n" (C.atomicNumber el) (C.name el)
    False -> printf "%s = %3d %s\n" (decodeUtf8 a) (C.atomicNumber el) (C.name el)

run a@Args { quiet, mode = Bruteforce ns e_symb } = do
  e <- case C.elementBySymbol e_symb of
    C.Unknown -> do printf "I don't know about %s.\n" e_symb
                    exitFailure
    e' -> pure e'
  bs <- bruteforce quiet ns e
  case quiet of
    True -> printf "%s\n" (decodeUtf8 $ render bs)
    False -> do
      printf "\n"
      run $ a { mode = Hash . render $ bs }

hashOptions :: Parser Mode
hashOptions = Hash <$>
  argument str (metavar "<data>")

searchOptions :: Parser Mode
searchOptions = Bruteforce <$> argument str (metavar "<namespace>")
                           <*> argument str (metavar "<element>")

parseMode :: Parser Mode
parseMode =
  hsubparser
    (  command "hash" (info hashOptions
         (progDesc "Hash a string into an element using Blake2b_512."))
    <> command "search" (info searchOptions
         (progDesc "Find a hexadecimal string of length 32 such that it, its 8 character prefix and its 4 character prefix hash to a given element."))
    )

opts :: Parser Args
opts = do
  quiet <- switch (  long "quiet"
                  <> short 'q'
                  <> help "Be quiet and only print the full result"
                  )
  mode <- parseMode
  pure Args {..}

main :: IO ()
main = customExecParser (prefs showHelpOnError) o >>= run
  where o = info (helper <*> opts) (  fullDesc
                                   <> progDesc "Generate hostnames from random data.")
