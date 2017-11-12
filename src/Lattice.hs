module Lattice where

import Control.Lens

import Data.Monoid ((<>))

-- TODO: This import probably isn't even needed. Leaving for now to force
-- pack/unpack and to try and keep a data interface for when typed edges are
-- needed later (e.g. fire triple bond).
import Data.Graph

import Hex
import Types

newtype Lattice = Lattice (Graph, Vertex -> (Element, Position, [Position]), Position -> Maybe Vertex)

instance Show Lattice where
  show = show . unpackLattice

-- TODO: should do undirected comparison, rather than directed
instance Eq Lattice where
  g1 == g2 = unpackLattice g1 == unpackLattice g2

data LatticeGraph = LatticeGraph [(Position, Element)] [(Position, Position)]

packLattice :: [(Element, Position, [Position])] -> Lattice
packLattice = Lattice . graphFromEdges

fuse :: Lattice -> Lattice -> Placement -> Position -> Lattice
fuse = undefined

unfuse :: Lattice -> Lattice -> Placement -> Position -> Lattice
unfuse = undefined

footprint :: Lattice -> [Position]
footprint (Lattice (g, vf, _)) = fmap (view _2 . vf) (vertices g)

absoluteLattice :: Placement -> Lattice -> Lattice
absoluteLattice (Placement anchor r) =
  packLattice . fmap (offset . rotate) . unpackLattice

  where
    rotate :: (Element, Position, [Position]) -> (Element, Position, [Position])
    rotate (e, p, edges) =
      (e, rotateHexAroundOrigin r p, fmap (rotateHexAroundOrigin r) edges)

    offset :: (Element, Position, [Position]) -> (Element, Position, [Position])
    offset (e, p, edges) =
      (e, anchor <> p, fmap (anchor <>) edges)

unpackLattice :: Lattice -> [(Element, Position, [Position])]
unpackLattice (Lattice (g, vf, _)) = fmap vf (vertices g)

rotateHexAroundOrigin :: Radians -> Position -> Position
rotateHexAroundOrigin r p =
  let Position x y = p in
  let (Radians r') = r in
  let (px, py) = hexToPixel (x, y) in
  let px' = px * cos r' - py * sin r' in
  let py' = py * cos r' + px * sin r' in
  let (x', y') = pixelToHex (px', py') in

  Position x' y'

