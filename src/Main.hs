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
import Data.Binary
import Data.Binary.Get

resourceFilePaths :: [FilePath]
resourceFilePaths = 
  [
    "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.001",
    "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.002",
    "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.003",
    "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.004"
  ]

levelFiles :: IO [TIMFile]
levelFiles = 
  do resourceFiles <- mapM decodeFile resourceFilePaths
     return $ concatMap files resourceFiles

getMany :: (Binary a, Enum n) => Get n -> Get [a]
getMany getCount = 
  do count <- getCount
     mapM (\_ -> get) [toEnum 1..count]

getAdInfinitum :: Binary a => Get [a]
getAdInfinitum = 
  do b <- isEmpty
     if b
     then return []
     else do x <- get
             xs <- getAdInfinitum
             return (x : xs)

unsupportedPut = error "We do not support put."

getTIMFileName = 
  do (filename :: [Word8]) <- mapM (\_ -> get) [0..12]
     return $ Bytes.pack $ takeWhile (/= 0) filename

data TIMFile = TIMFile {filename :: ByteString, filecontents :: [Word8]}
instance Binary TIMFile where
  get = do
    filename <- getTIMFileName
    filecontents <- getMany getWord32le
    return $ TIMFile filename filecontents
  put = unsupportedPut

data TIMResourceFile = TIMResourceFile {files :: [TIMFile]}
instance Binary TIMResourceFile where
  get = do files <- getAdInfinitum
           return $ TIMResourceFile files
  put = unsupportedPut

main :: IO ()
main = do
  putStrLn "Enter the pathname of the resource file you would like extracted."