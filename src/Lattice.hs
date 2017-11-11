module Lattice where

import Control.Lens

-- TODO: This import probably isn't even needed. Leaving for now to force
-- pack/unpack and to try and keep a data interface for when typed edges are
-- needed later (e.g. fire triple bond).
import Data.Graph

import Types

data Lattice = Lattice (Graph, Vertex -> (Element, Position, [Position]), Position -> Maybe Vertex) 

instance Show Lattice where
  show (Lattice (g, _, _)) = show g

instance Eq Lattice where
  Lattice (g1, _, _) == Lattice (g2, _, _) = g1 == g2

data LatticeGraph = LatticeGraph [(Position, Element)] [(Position, Position)]

packLattice :: [(Element, Position, [Position])] -> Lattice
packLattice = Lattice . graphFromEdges

fuse :: Lattice -> Lattice -> Placement -> Position -> Lattice
fuse = undefined

unfuse :: Lattice -> Lattice -> Placement -> Position -> Lattice
unfuse = undefined

footprint :: Lattice -> [Position]
footprint (Lattice (g, vf, _)) = fmap (view _2 . vf) (vertices g)

unpackLattice :: Lattice -> [(Element, Position, [Position])]
unpackLattice (Lattice (g, vf, _)) = fmap vf (vertices g)
