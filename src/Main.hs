{-# LANGUAGE DuplicateRecordFields, DeriveGeneric #-}
module Main (main) where

-- The understanding of TIM file formats is derived from 
-- https://moddingwiki.shikadi.net/wiki/The_Incredible_Machine.
-- In particular:
-- https://moddingwiki.shikadi.net/wiki/TIM_Resource_Format
-- https://moddingwiki.shikadi.net/wiki/The_Incredible_Machine_Level_Format

import qualified Data.ByteString.Lazy as Bytes
import Control.Monad
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Internal(c2w, w2c)
import Data.Word
import Data.Binary
import Data.Binary.Get
import GHC.Generics
import System.FilePath

-- All comments tested only against TEMIM.

{- 
  In this first section, we extract the data stored in the resource files.
  (RESOURCE.001 through RESOURCE.004)

  After extraction, the level files are stored as L#.LEV.
  There is also an NL17.LEV, apparently superseding L17.LEV.
  Perhaps this is because L17.LEV contains a typo in its goal description.
-}

myResourceFiles = [
  "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.001",
  "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.002",
  "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.003",
  "/Users/sridharramesh/Emu/DOS/TIM/RESOURCE.004"
  ]

gameFiles :: [FilePath] -> IO [TIMFile]
gameFiles resourceFilePaths = 
  do resourceFiles <- mapM decodeFile resourceFilePaths
     return $ concatMap files resourceFiles

levelFiles :: [FilePath] -> IO [TIMFile]
levelFiles resourceFilePaths = 
  do gameFiles' <- gameFiles resourceFilePaths
     return $ filter ((stringToByteString ".LEV" `Bytes.isSuffixOf`) . filename) gameFiles'

outputGameFiles :: [FilePath] -> FilePath -> IO ()
outputGameFiles resourceFilePaths outputDir = do 
  gameFiles' <- gameFiles resourceFilePaths
  mapM_ (flip outputGameFile outputDir) gameFiles'

outputGameFile :: TIMFile -> FilePath -> IO ()
outputGameFile file outputDir = Bytes.writeFile 
  (joinPath [outputDir, byteStringToString $ filename $ file])
  (filecontents file)


stringToByteString :: String -> ByteString
stringToByteString = Bytes.pack . map c2w

byteStringToString :: ByteString -> String
byteStringToString = map w2c . Bytes.unpack

unsupportedPut = error "We do not support put."

data NullTerminatedString = NullTerminatedString {byteString :: ByteString}
  deriving (Generic, Show)
instance Binary NullTerminatedString where
  get = do g <- getNullTerminatedString
           return $ NullTerminatedString g
  put = unsupportedPut

getNullTerminatedString :: Get ByteString
getNullTerminatedString =
  do unpackedString <- getNullTerminatedString'
     return $ Bytes.pack unpackedString
    where 
      getNullTerminatedString' = 
        do byte <- getWord8
           if byte == 0
           then return []
           else do bytes <- getNullTerminatedString'
                   return (byte:bytes)

data IndicatedMany n a = IndicatedMany {count :: n, list :: [a]}
  deriving (Generic, Show)
instance (Binary n, Enum n, Binary a) => Binary (IndicatedMany n a) where
  get = do (count, list) <- getIndicatedMany' get
           return $ IndicatedMany count list
  put = unsupportedPut

getIndicatedMany' :: (Binary a, Enum n) => Get n -> Get (n, [a])
getIndicatedMany' getCount = 
  do count <- getCount
     list <- getMany (fromEnum count)
     return $ (count, list)

getIndicatedMany :: (Binary a, Enum n) => Get n -> Get [a]
getIndicatedMany getCount = 
  do (_, list) <- getIndicatedMany' getCount
     return $ list

getMany :: Binary a => Int -> Get [a]
getMany n = mapM (\_ -> get) [1..n]

getAdInfinitum :: Binary a => Get [a]
getAdInfinitum = 
  do b <- isEmpty
     if b
     then return []
     else do x <- get
             xs <- getAdInfinitum
             return (x : xs)

getTIMFileName :: Get ByteString
getTIMFileName = 
  do (filename :: [Word8]) <- getMany 13
     return $ Bytes.pack $ takeWhile (/= 0) filename

data TIMFile = TIMFile {filename :: ByteString, filecontents :: ByteString}
  deriving (Generic, Show)
instance Binary TIMFile where
  get = do
    filename <- getTIMFileName
    filecontents <- getIndicatedMany getWord32le
    return $ TIMFile filename (Bytes.pack filecontents)
  put = unsupportedPut

data TIMResourceFile = TIMResourceFile {files :: [TIMFile]}
  deriving (Generic, Show)
instance Binary TIMResourceFile where
  get = do files <- getAdInfinitum
           return $ TIMResourceFile files
  put = unsupportedPut

data TIMLevelVersion = TIM_Demo | TIM | TEMIM | TIM2_MinusMinus | TIM2_Minus | TIM2 | TIM3
  deriving (Generic, Show)
instance Binary TIMLevelVersion where
  get = 
    do magicNumber :: (Word8, Word8, Word8, Word8) <- get
       case magicNumber of
         (0xED, 0xAC, 0x00, 0x01) -> return TIM_Demo
         (0xED, 0xAC, 0x02, 0x01) -> return TIM
         (0xEE, 0xAC, 0x05, 0x01) -> return TEMIM
         (0xEF, 0xAC, 0x11, 0x01) -> return TIM2_MinusMinus
         (0xEF, 0xAC, 0x12, 0x01) -> return TIM2_Minus
         (0xEF, 0xAC, 0x13, 0x01) -> return TIM2
         (0xEF, 0xAC, 0x14, 0x01) -> return TIM3
         _ -> fail "Unrecognized TIM version!"
  put = unsupportedPut

data StandardHeader = StandardHeader
  {
    version :: TIMLevelVersion, 
    title :: NullTerminatedString, 
    goal :: NullTerminatedString
  }
  deriving (Generic, Show)
instance Binary StandardHeader

-- TODO: Add support for parsing these with 
data TIMLevel' headerType = TIMLevel'
  {
    header :: headerType
  }
  deriving (Generic, Show)
instance (Binary headerType) => Binary (TIMLevel' headerType)
type TIMLevel = TIMLevel' StandardHeader

main :: IO ()
main = do
  putStrLn "Command line support not yet ready."