module Main (main) where

-- TODO: Error handling throughout.

-- The understanding of TIM file formats is derived from 
-- https://moddingwiki.shikadi.net/wiki/The_Incredible_Machine.
-- In particular:
-- https://moddingwiki.shikadi.net/wiki/TIM_Resource_Format
-- https://moddingwiki.shikadi.net/wiki/The_Incredible_Machine_Level_Format

-- We reinvent the wheel on many things to keep dependencies low.
-- But we do use the ByteString library, for no good reason.
-- Ordinarily, I would trust others to have done much more performance optimizing
-- than I could or would. However, all TIM files are so small, this doesn't matter.
-- TODO: Clean up file parsing code, probably should be using the state monad.

import qualified Data.ByteString as Bytes
import Data.ByteString(ByteString)
import Data.Word

x :: IO [ByteString]
x = mapM Bytes.readFile 
  [
    "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.001",
    "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.002",
    "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.003",
    "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.004"
  ]

nullByte :: Word8
nullByte = 0

isEmpty :: ByteString -> Bool
isEmpty = Bytes.null

bytesTakeDrop :: Int -> ByteString -> (ByteString, ByteString)
bytesTakeDrop n bytes = (Bytes.take n bytes, Bytes.drop n bytes)

parseFileName :: ByteString -> (ByteString, ByteString)
parseFileName (bytes :: ByteString) = 
  let (first13, remainder) = bytesTakeDrop 13 bytes
      (beforeNull, nullOnwards) = Bytes.break (nullByte ==) first13
  in if isEmpty nullOnwards
     then error "Tried parsing a filename, but didn't find a null in 13 bytes."
     else (beforeNull, remainder)

convertBytesToUINTLE :: ByteString -> Int
convertBytesToUINTLE bytes = 
  let bytesAsInts = map fromEnum $ Bytes.unpack bytes
      significances = iterate (256*) 1
  in sum $ zipWith (*) bytesAsInts significances

parseUINT32LE :: ByteString -> (Int, ByteString)
parseUINT32LE (bytes :: ByteString) 
  | Bytes.length bytes < 4 = error "Tried parsing a UINT32LE, but had under 4 bytes."
  | otherwise = 
    let (prefix, remainder) = bytesTakeDrop 4 bytes
        num = convertBytesToUINTLE prefix
    in (num, remainder)

parseTIMFile :: ByteString -> ((ByteString, ByteString), ByteString)
parseTIMFile (bytes :: ByteString) = 
  let (filename, remainder) = parseFileName bytes
      (filesize, remainder2) = parseUINT32LE remainder
  in if Bytes.length remainder2 < filesize
     then error "Tried parsing a file, but the stated file size was longer than provided."
     else let (filedata, remainder3) = bytesTakeDrop filesize remainder2
          in ((filename, filedata), remainder3)

parseTIMFiles :: ByteString -> [(ByteString, ByteString)]
parseTIMFiles (bytes :: ByteString)
  | isEmpty bytes = []
  | otherwise = 
    let (file, remainder) = parseTIMFile bytes
    in file : (parseTIMFiles remainder)

main :: IO ()
main = do
  putStrLn "Enter the pathname of the resource file you would like extracted."