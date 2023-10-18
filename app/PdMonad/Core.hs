{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PdMonad.Core where

import Data.Function (on)
import Data.Graph.Inductive (Gr, Graph (labNodes, mkGraph))
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T


------------------------------------------------------------------------
-- | type creations


data PdObject = PdObject
  { objectId :: Maybe Int,
    objectType :: T.Text,
    objectCoordinates :: (Double, Double),
    objectArguments :: T.Text
  }
  deriving (Show, Eq)


data PdConnection = PdConn
  { source :: Maybe Int,
    outlet :: Int,
    sink :: Maybe Int,
    inlet :: Int
  }
  deriving (Show)


data PdPatch = PdPatch
  { pdObjects :: [PdObject],
    pdConnections :: [PdConnection]
  }
  deriving (Show)


------------------------------------------------------------------------
-- | patch synthesis


writePatch :: String -> [PdPatch] -> IO ()
writePatch name pdPatch = do
  placedPatch <- placement pdPatch
  let textObjects = concatMap (map objectToText . pdObjects) placedPatch
      textConnections = concatMap (map connectionToText . pdConnections) pdPatch
      canvas = [T.pack "#N canvas 0 50 450 300 12;"]
  T.writeFile name $ T.unlines $ concat [canvas, textObjects, textConnections]


placement :: [PdPatch] -> IO [PdPatch]
placement pdPatch = do
  let numberedObjects = nub . concatMap pdObjects $ pdPatch
      connections = concatMap pdConnections pdPatch
  placedObjects <- getCoordinatesFromGraph $ patchToGraph numberedObjects connections
  let zippedCoords = zipWith (\x j -> j {objectCoordinates = x}) placedObjects numberedObjects
  return [PdPatch zippedCoords (concatMap pdConnections pdPatch)]


objectToText :: PdObject -> T.Text
objectToText object =
  let (x, y) = objectCoordinates object
      fields = [objectType object, T.pack $ show x, T.pack $ show y, objectArguments object]
   in T.append (T.unwords fields) ";"


connectionToText :: PdConnection -> T.Text
connectionToText connection =
  T.pack $
    unwords
      [ "#X connect",
        show (fromMaybe 0 $ source connection),
        show (outlet connection),
        show (fromMaybe 0 $ sink connection),
        show (inlet connection)
      ]
      ++ ";"


------------------------------------------------------------------------
-- | custom operators


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


------------------------------------------------------------------------
-- | graph methods


patchToGraph :: [PdObject] -> [PdConnection] -> Gr Int Int
patchToGraph indexedObjects connections =
  let nodes = fmap ((\i -> (i, i)) . fromMaybe 0 . objectId) indexedObjects
      edges = [(fromMaybe 0 i, fromMaybe 0 j, 0) | PdConn i _ j _ <- connections]
   in mkGraph nodes edges


-- | labNodes :: gr a b -> [LNode a]
-- LNode a :: (Node, a)
-- Node :: Int
-- Layout = GraphvizCommand
-- in [Attributes]
getCoordinatesFromGraph :: Gr Int Int -> IO [(Double, Double)]
getCoordinatesFromGraph gr = do
  layoutedGraph <- graphToGraph nonClusteredParams {globalAttributes = ga} gr
  return . map getXY . nodeToCoords $ layoutedGraph
  where
    nodeToCoords = concatMap (fst . snd) . labNodes
    ga = [GraphAttrs [Layout Dot, RankDir FromBottom, NodeSep 1, RankSep [0.02]]]
    getXY :: Attribute -> (Double, Double)
    getXY (Pos (PointPos (Point {xCoord = x, yCoord = y}))) = (x, y)
