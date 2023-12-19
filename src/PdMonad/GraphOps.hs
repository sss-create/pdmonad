module PdMonad.GraphOps where

import Data.Graph.Inductive (Gr, Graph (labNodes, mkGraph))
import Data.GraphViz
import Data.GraphViz.Attributes.Complete


graphAttributes :: [GlobalAttributes]
graphAttributes = [GraphAttrs [Layout Dot, RankDir FromBottom, NodeSep 1, RankSep [0.02]]]


-- | labNodes :: gr a b -> [LNode a]
-- LNode a :: (Node, a)
-- Node :: Int
-- Layout = GraphvizCommand
-- in [Attributes]
getCoordinatesFromGraph :: Gr Int Int -> IO [(Double, Double)]
getCoordinatesFromGraph gr = do
  layoutedGraph <- graphToGraph nonClusteredParams {globalAttributes = graphAttributes} gr
  let nodeToCoords = concatMap (fst . snd) . labNodes
  pure . map xyCoordinates . nodeToCoords $ layoutedGraph


xyCoordinates :: Attribute -> (Double, Double)
xyCoordinates (Pos (PointPos (Point {xCoord = x, yCoord = y}))) = (x, y)
