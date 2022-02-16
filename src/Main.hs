{-# language BangPatterns, NamedFieldPuns, RecordWildCards, ApplicativeDo #-}
module Main where

import           Control.Category ((>>>))
import           Control.Monad (when, unless)
import           Data.Traversable (traverse)
import           Data.Foldable (traverse_)
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
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Char (toLower)

byteStringToElement :: BS.ByteString -> C.Element
byteStringToElement =     hashWith Blake2b_512
                      >>> convert
                      >>> BS.foldl1' xor
                      >>> fromEnum
                      >>> (`mod` 118)
                      >>> (+ 1)
                      >>> C.element
                      >>> fromJust

render :: BS.ByteString -> BS.ByteString
render = BL.byteStringHex >>> BL.toLazyByteString >>> BL.toStrict

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
    output :: Word64 -> BS.ByteString -> Int -> Int -> Int -> IO ()
    output n bs t1 t2 t3 = do
      printf "\r\x1b[K%10d %s (%s) (%s): %3d %d %d"
        n
        (decodeUtf8 $ render bs)
        (decodeUtf8.render $ BS.take 4 bs)
        (decodeUtf8.render $ BS.take 2 bs)
        t1 t2 t3
      hFlush stdout

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

bruteforceAll :: Bool -> BS.ByteString -> IO (M.Map C.Element BS.ByteString)
bruteforceAll quiet ns = go M.empty 0
  where
    noOfElements = length C.ptable

    output :: Int -> Word64 -> BS.ByteString -> Int -> Int -> Int -> IO ()
    output total n bs t1 t2 t3 = do
      printf "\r\x1b[K%3d %10d %s (%s) (%s): %3d %d %d"
        total
        n
        (decodeUtf8 $ render bs)
        (decodeUtf8.render $ BS.take 4 bs)
        (decodeUtf8.render $ BS.take 2 bs)
        t1 t2 t3
      hFlush stdout

    go :: M.Map C.Element BS.ByteString -> Word64 -> IO (M.Map C.Element BS.ByteString)
    go !prevMap !n
      | M.size prevMap == noOfElements = pure prevMap
      | otherwise = do
        let (bs, t1, t2, t3) = tryOnce ns n
        unless quiet $ when (n `mod` 0x100 == 0 && n `mod` 0xff == 0) $
          output (M.size prevMap) n bs (C.atomicNumber t1) (C.atomicNumber t2) (C.atomicNumber t3)
        if (t1 == t2) && (t2 == t3)
          then do
            let newMap = M.insertWith (\_ old -> old) t1 bs prevMap
            unless quiet $ output (M.size newMap) n bs (C.atomicNumber t1) (C.atomicNumber t2) (C.atomicNumber t3)
            if (M.size newMap == noOfElements)
              then pure newMap
              else go newMap (n+1)
          else go prevMap (n+1)

data Mode = Hash BS.ByteString | Bruteforce BS.ByteString String | NixTable BS.ByteString

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
    Nothing -> case C.elementByName e_symb of
      Nothing -> do printf "I don't know about %s.\n" e_symb
                    exitFailure
      Just e' -> pure e'
    Just e' -> pure e'
  bs <- bruteforce quiet ns e
  case quiet of
    True -> printf "%s\n" (decodeUtf8 $ render bs)
    False -> do
      printf "\n"
      run $ a { mode = Hash . render $ bs }

run a@Args { quiet, mode = NixTable ns } = do
  bss <- bruteforceAll quiet ns
  printf "\n{\n"
  _ <- M.traverseWithKey (\e bs -> printf "\"%s\" = \"%s\";\n" (toLower <$> C.name e) (decodeUtf8 $ render bs)) bss
  printf "}\n"

hashOptions :: Parser Mode
hashOptions = Hash <$>
  argument str (metavar "<data>")

searchOptions :: Parser Mode
searchOptions = Bruteforce <$> argument str (metavar "<namespace>")
                           <*> argument str (metavar "<element>")

nixTableOptions :: Parser Mode
nixTableOptions = NixTable <$> argument str (metavar "<namespace>")

parseMode :: Parser Mode
parseMode =
  hsubparser
    (  command "hash" (info hashOptions
         (progDesc "Hash a string into an element using Blake2b_512."))
    <> command "search" (info searchOptions
         (progDesc "Find a hexadecimal string of length 32 such that it, its 8 character prefix and its 4 character prefix hash to a given element."))
    <> command "nix-table" (info nixTableOptions
         (progDesc "Generate a table in .nix format for all elements in a given namespace."))
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
