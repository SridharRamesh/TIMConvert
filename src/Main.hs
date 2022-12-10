{-# LANGUAGE DuplicateRecordFields, GeneralisedNewtypeDeriving #-}
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
  get = getNullTerminatedString >>= (return . NullTerminatedString)
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
     list <- getMany count
     return $ (count, list)

getIndicatedMany :: (Binary a, Enum n) => Get n -> Get [a]
getIndicatedMany getCount = 
  do (_, list) <- getIndicatedMany' getCount
     return $ list

getMany :: (Binary a, Enum n) => n -> Get [a]
getMany count = mapM (\_ -> get) [1..fromEnum count]

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
  get = getAdInfinitum >>= (return . TIMResourceFile)
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
         _ -> fail ("Unrecognized TIM version!" ++ show magicNumber)
  put = unsupportedPut

newtype TIMWord = TIMWord Word16
  deriving (Generic, Show, Eq, Enum, Num)
instance Binary TIMWord where
  get = getWord16le >>= (return . TIMWord)
  put = unsupportedPut

noneValue = TIMWord (-1)

-- By default, TIMWords should be interpreted as unsigned.
-- In a few cases, they are interpreted as signed.
type SignedTIMWord = TIMWord

-- TODO: Add support for all the TEMIM file formats
-- (Official levels, freeform machines, title, and credits)
data StandardHeader = StandardHeader
  {
    version :: TIMLevelVersion, 
    title :: NullTerminatedString, 
    goal :: NullTerminatedString,
    bonus1 :: TIMWord,
    bonus2 :: TIMWord,
    pressure :: SignedTIMWord,
    gravity :: SignedTIMWord,
    unknown8 :: TIMWord,
    unknown10 :: TIMWord,
    music :: TIMWord,
    numFixedParts :: TIMWord,
    numMovingParts :: TIMWord,
    numBinParts :: TIMWord
  }
  deriving (Generic, Show)
instance Binary StandardHeader

data EndOfFile = EndOfFile
  deriving (Generic, Show)
instance Binary EndOfFile -- TODO: Implement this properly

data TIMLevel = TIMLevel
  {
    header :: StandardHeader,
    unbinnedParts :: [TIMPart],
    freeformBinBit :: TIMWord,
    binnedParts :: [TIMPart]
  }
  deriving (Generic, Show)
instance Binary TIMLevel where
  get = do
    header <- get
    unbinnedParts <- getMany (numFixedParts header + numMovingParts header)
    freeformBinBit <- get
    binnedParts <- getMany (numBinParts header)
    return $ TIMLevel header unbinnedParts freeformBinBit binnedParts

data ConnectionPoint = ConnectionPoint {unknown :: TIMWord, x :: Word8, y :: Word8}
  deriving (Generic, Show)
instance Binary ConnectionPoint

type PartType = TIMWord
type PartIndex = TIMWord

type AlwaysOne = TIMWord
type AlwaysZero = TIMWord
type AlwaysNone = TIMWord

data ConnectedTo = ConnectedTo {part1 :: PartIndex, part2 :: PartIndex}
  deriving (Generic, Show)
instance Binary ConnectedTo

data TIMPartHeader = TIMPartHeader
  {
    partType :: PartType,
    flags1 :: TIMWord,
    flags2 :: TIMWord,
    flags3 :: TIMWord,
    appearance :: TIMWord,
    unknown10 :: TIMWord,
    width1 :: TIMWord,
    height1 :: TIMWord,
    width2 :: TIMWord,
    height2 :: TIMWord,
    xPos :: SignedTIMWord,
    yPos :: SignedTIMWord,
    state :: TIMWord -- Length for a rope
  }
  deriving (Generic, Show)
instance Binary TIMPartHeader

data TIMPartStandardSuffix = TIMPartStandardSuffix
  {
    beltConnect :: ConnectionPoint,
    beltGap :: TIMWord,
    firstRopeConnectionPoint :: ConnectionPoint,
    secondRopeConnectionPoint :: ConnectionPoint,
    connectedParts :: Connections
  }
  deriving (Generic, Show)
instance Binary TIMPartStandardSuffix

data TIMPartSuffix = Standard TIMPartStandardSuffix | Belt BeltSuffix | Rope RopeSuffix | Pulley PulleySuffix
  deriving (Generic, Show)

data Connections = Connections {ropeConnected :: ConnectedTo, plugged :: ConnectedTo}
  deriving (Generic, Show)
instance Binary Connections

data TIMPart = TIMPart TIMPartHeader TIMPartSuffix
  deriving (Generic, Show)
instance Binary TIMPart where
  get = do 
    header <- get
    suffix <- getSuffixGivenPartType (partType header)
    return $ TIMPart header suffix
  put = unsupportedPut

beltType = 8
ropeType = 10
pulleyType = 7

getSuffixGivenPartType partType
  | partType == beltType 
    = get >>= (return . Belt)
  | partType == ropeType
    = get >>= (return . Rope)
  | partType == pulleyType 
    = get >>= (return . Pulley)
  | otherwise 
    = get >>= (return . Standard)

data BeltSuffix = BeltSuffix
  {
    unknownBeltField0 :: AlwaysOne,
    unknownBeltField1 :: AlwaysZero,
    unknownBeltField2 :: AlwaysZero,
    thisBeltConnects :: ConnectedTo,
    unknownBeltField3 :: AlwaysZero,
    unknownBeltField4 :: AlwaysZero,
    unknownBeltField5 :: AlwaysZero,
    unknownBeltField6 :: AlwaysZero,
    connectedParts :: Connections -- Always none
  }
  deriving (Generic, Show)
instance Binary BeltSuffix

data RopeSuffix = RopeSuffix
  {
    unknownRopeField0 :: AlwaysZero,
    unknownRopeField1 :: AlwaysZero,
    unknownRopeField2 :: AlwaysZero,
    unknownRopeField3 :: AlwaysOne,
    unknownRopeField4 :: AlwaysZero,
    thisRopeConnects :: ConnectedTo,
    slotInFirstConnection :: Word8,
    slotInSecondConnection :: Word8,
    unknownRopeField5 :: AlwaysZero,
    unknownRopeField6 :: AlwaysZero,
    connectedParts :: Connections -- Always none
  }
  deriving (Generic, Show)
instance Binary RopeSuffix

data PulleySuffix = PulleySuffix
  {
    unknownPulleyField0 :: AlwaysZero,
    unknownPulleyField1 :: AlwaysZero,
    unknownPulleyField2 :: AlwaysZero,
    unknownPulleyField3 :: AlwaysOne,
    firstRopeConnectionPoint :: ConnectionPoint,
    unknownPulleyField4 :: AlwaysNone,
    unknownPulleyField5 :: AlwaysNone,
    unknownPulleyField6 :: AlwaysZero,
    unknownPulleyField7 :: AlwaysZero,
    secondRopeConnectionPoint :: ConnectionPoint,
    connectedParts :: Connections,
    rope :: PartIndex -- Set to None in T(EM)IM, only used in TIM 2/3.
  }
  deriving (Generic, Show)
instance Binary PulleySuffix

data ProgrammableBallSuffix = ProgrammableBallSuffix
  {
    standardSuffix :: TIMPartSuffix,
    density :: TIMWord,
    elasticity :: TIMWord,
    friction :: TIMWord,
    gravityBuoyancy :: SignedTIMWord,
    mass :: TIMWord,
    appearance :: TIMWord
  }
  deriving (Generic, Show)

main :: IO ()
main = do
  putStrLn "Command line support not yet ready."