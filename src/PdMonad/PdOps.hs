{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PdMonad.PdOps where

import PdMonad.Pd
import PdMonad.GraphOps

import Data.List (nub)
import Data.Maybe (fromMaybe)

import Data.Text qualified as T
import Data.Text.IO qualified as T

import Data.Graph.Inductive 


class Chainable a where
  (-->) :: a -> PdObject -> PdPatch


instance Chainable PdObject where
  a --> b =
    let conns = PdConn (objectId a) 0 (objectId b) 0
    in PdPatch [a,b] [conns]


instance Chainable PdPatch where
  PdPatch objects connections --> nextObject =
    let newConnection = PdConn (objectId $ last objects) 0 (objectId nextObject) 0
    in PdPatch (objects ++ [nextObject]) (connections ++ [newConnection])


infixr 1 #
(#) :: PdObject -> Int -> PdObject
object # n = object {objectId = Just n}


preparePatch :: [PdPatch] -> PdPatch
preparePatch pdPatch =
  let uniqueObjs = nub . concatMap pdObjects $ pdPatch
      connections = concatMap pdConnections pdPatch
  in PdPatch uniqueObjs connections


writePatch :: String -> PdPatch -> IO ()
writePatch name pdPatch = do
  placedPatch <- placeObjects pdPatch
  let textObjects = map objectToText . pdObjects $ placedPatch
      textConnections = map connectionToText . pdConnections $ pdPatch
      canvas = [T.pack "#N canvas 0 50 450 300 12;"]
  T.writeFile name $ T.unlines $ concat [canvas, textObjects, textConnections]


placeObjects :: PdPatch -> IO PdPatch
placeObjects pdPatch = do
  placedObjects <- getCoordinatesFromGraph $ patchToGraph pdPatch
  let zippedCoords = zipWith (\x j -> j {objectCoordinates = x}) placedObjects $ pdObjects pdPatch
  pure . PdPatch zippedCoords $ pdConnections pdPatch


patchToGraph :: PdPatch -> Gr Int Int
patchToGraph p =
  let nodes = map ((\i -> (i,i)) . fromMaybe 0 . objectId) $ pdObjects p 
      edges = [(fromMaybe 0 i, fromMaybe 0 j, 0) | PdConn i _ j _ <- pdConnections p]
  in mkGraph nodes edges


objectToText :: PdObject -> T.Text
objectToText object =
  let (x, y) = objectCoordinates object
      fields = [objectType object, T.pack $ show x, T.pack $ show y, objectArguments object]
  in T.append (T.unwords fields) ";"


connectionToText :: PdConnection -> T.Text
connectionToText connection = T.pack $ unwords
  [ "#X connect"
  , show (fromMaybe 0 $ source connection)
  , show (outlet connection)
  , show (fromMaybe 0 $ sink connection)
  , show (inlet connection)
  ] ++ ";"
