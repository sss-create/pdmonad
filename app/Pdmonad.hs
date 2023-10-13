{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Pdmonad where

import Data.Maybe (fromMaybe)
import Data.List (nubBy)
import Data.Function (on)
import Data.Text qualified as T
import Data.Text.IO qualified as T


data PdObject = PdObject
  {
    objectId :: Maybe Int
  , objectType :: T.Text
  , objectCoordinates :: (Int, Int)
  , objectArguments :: T.Text
  } deriving (Show, Eq)


data PdConnection = PdConn
  {
    source :: Maybe Int
  , outlet :: Int
  , sink :: Maybe Int
  , inlet :: Int
  } deriving (Show)


data PdPatch = PdPatch
  {
    pdObjects :: [PdObject]
  , pdConnections :: [PdConnection]
  } deriving (Show)


column :: PdPatch
column = PdPatch [] []


defPdObject :: PdObject
defPdObject = PdObject Nothing "#X obj" (50, 75) ""


obj :: T.Text -> PdObject
obj args = defPdObject {objectArguments = args}


mult :: Float -> PdObject
mult args = defPdObject {objectArguments = T.concat ["* ", T.pack $ show args]}


msg :: T.Text -> PdObject
msg args = defPdObject {objectType = "#X msg", objectArguments = args}


pdprint :: PdObject
pdprint = defPdObject {objectArguments = "print"}


loadbang :: PdObject
loadbang = defPdObject {objectArguments = "loadbang"}


bang :: PdObject
bang =
  let args = "bng 19 250 50 0 empty empty empty 0 -10 0 12 #fcfcfc #000000 #000000;"
  in defPdObject {objectArguments = args}


toggle :: PdObject
toggle =
  let args = "tgl 19 0 empty empty empty 0 -10 0 12 #fcfcfc #000000 #000000 0 1;"
  in defPdObject {objectArguments = args}


number :: PdObject
number =
  let args = "5 0 0 0 - - - 0;"
  in defPdObject {objectType = "#X floatatom", objectArguments = args}


updateObjectCoordinates :: (Int, PdObject) -> PdObject
updateObjectCoordinates (mid, mobj) =
  mobj {objectCoordinates = (100 * mid, maybe 0 (* 25) $ objectId mobj)}


objectToText :: PdObject -> T.Text
objectToText object =
  let (x, y) = objectCoordinates object
      fields = [objectType object, T.pack $ show x, T.pack $ show y, objectArguments object]
  in  T.append (T.unwords fields) ";"


connectionToText :: PdConnection -> T.Text
connectionToText connection =
  T.pack $ unwords
      [ "#X connect",
        show (fromMaybe 0 $ source connection),
        show (outlet connection),
        show (fromMaybe 0 $ sink connection),
        show (inlet connection)
      ] ++ ";"


writePatch :: String -> [PdPatch] -> IO ()
writePatch name pdPatch =
  let textObjects = concatMap (map objectToText . pdObjects) $ placement pdPatch
      textConnections = concatMap (map connectionToText . pdConnections) pdPatch
      canvas = [T.pack "#N canvas 0 50 450 300 12;"]
  in T.writeFile name $ T.unlines $ concat [canvas, textObjects, textConnections]


-- https://stackoverflow.com/questions/58716167/appending-index-to-a-list-of-lists-in-haskell
placement :: [PdPatch] -> [PdPatch]
placement pdPatch =
  let objects = map pdObjects pdPatch
      numberedObjects = nubBy ((==) `on` snd) . concat . zipWith (map . (,)) [0..] $ objects
      placedObjects = fmap updateObjectCoordinates numberedObjects
  in [PdPatch placedObjects (concatMap pdConnections pdPatch)]


infixl 5 -->
(-->) :: PdPatch -> PdObject -> PdPatch
PdPatch objects connections --> nextObject =
  let newObjectList = objects ++ [nextObject]
      newConnections =
        case objects of
          [] -> connections
          _ -> connections ++ [PdConn (objectId $ last objects) 0 (objectId nextObject) 0]
  in PdPatch newObjectList newConnections


(#) :: PdObject -> Int -> PdObject
object # n = object {objectId = Just n}
