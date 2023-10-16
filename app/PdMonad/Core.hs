{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module PdMonad.Core where

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


infixl 0 -->
(-->) :: PdPatch -> PdObject -> PdPatch
PdPatch objects connections --> nextObject =
  let newObjectList = objects ++ [nextObject]
      newConnections =
        case objects of
          [] -> connections
          _ -> connections ++ [PdConn (objectId $ last objects) 0 (objectId nextObject) 0]
  in PdPatch newObjectList newConnections


infixr 1 #
(#) :: PdObject -> Int -> PdObject
object # n = object {objectId = Just n}


newCol = PdPatch [] []


escapeSpecial :: T.Text -> T.Text
escapeSpecial n = 
  let specials = [",", "$"]
  in foldl (\t s -> T.replace s (T.append "\\" s) t) n specials
