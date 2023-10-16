module PdMonad.Graphviz where

import PdMonad.Core
import Data.GraphViz
import Data.Graph.Inductive ( Gr, Graph(labNodes, mkGraph) )
import Data.GraphViz.Attributes.Complete
    ( Attribute(Pos, Layout), Point(Point, xCoord, yCoord), Pos(PointPos) )


--eg :: Gr Int Int
--eg = mkGraph [(1, 1), (2,2), (3,3)] [(1,2,0), (1,3,0)]


getXY :: Attribute -> (Double, Double)
getXY (Pos (PointPos (Point {xCoord = x, yCoord = y}))) = (x,y)


--patchToGraph :: PdPatch -> Gr Int Int
--patchToGraph = 


-- | labNodes :: gr a b -> [LNode a]
-- LNode a :: (Node, a)
-- Node :: Int
-- Layout = GraphvizCommand
-- in [Attributes]
graph :: Gr Int Int -> IO [(Double, Double)]
graph gr = do
    let nodeToCoords = concatMap (fst . snd) . labNodes
        ga = [GraphAttrs [Layout Fdp]]
    layoutedGraph <- graphToGraph nonClusteredParams { globalAttributes = ga } gr
    return . map getXY . nodeToCoords $ layoutedGraph
