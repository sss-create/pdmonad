module PdMonad.Graphviz where

import PdMonad.Core
import Data.GraphViz
import Data.Graph.Inductive ( Gr, Graph(labNodes, mkGraph) )
import Data.GraphViz.Attributes.Complete
    ( Attribute(Pos, Layout), Point(Point, xCoord, yCoord), Pos(PointPos) )
import Data.Maybe (fromMaybe)
