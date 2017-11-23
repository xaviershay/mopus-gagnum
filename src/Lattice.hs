module Lattice where

import Control.Lens

import Data.Monoid ((<>))

-- TODO: This import probably isn't even needed. Leaving for now to force
-- pack/unpack and to try and keep a data interface for when typed edges are
-- needed later (e.g. fire triple bond).
import qualified Data.Graph.Inductive as G
import qualified Data.Map as M

import Hex
import Types

newtype Lattice = Lattice (G.Gr (Element, Position) ()) deriving (Show)

--instance Show Lattice where
--  show = show . unpackLattice

-- TODO: should do undirected comparison, rather than directed
--instance Eq Lattice where
--  g1 == g2 = unpackLattice g1 == unpackLattice g2

data LatticeGraph = LatticeGraph [(Position, Element)] [(Position, Position)]

--toGraph :: Lattice -> Graph
--toGraph (Lattice (g, _, _)) = g

--extractPositions :: Lattice -> [Position]
--extractPositions l = map f . unpackLattice $ l
--  where
--    f (_, p, _) = p

-- TODO: Error on graphs with unconnected components?
packLattice :: [(Element, Position, [Position])] -> Lattice
packLattice xs = Lattice $ G.mkGraph nodes edges
  where
    nodeIndices = G.newNodes (length xs) (G.empty :: G.Gr (Element, Position) ())
    positionIndices = M.fromList $ zip (map (^. _2) xs) nodeIndices
    nodes = zip nodeIndices (map (\(a, b, _) -> (a, b)) xs)
    edges = concatMap (\(_, p, es) -> map (\e -> (nodeIndex p, nodeIndex e, ())) es) xs
    nodeIndex p = case M.lookup p positionIndices of
                    Just n  -> n
                    Nothing -> error "should never happen"

-- Pre-condition: l1 and l2 are absolutely positioned, and share no nodes
-- Pre-condition: p1 refers to an element in l1, p2 to l2
-- Post-condition: returned lattice is a single connected component
--fuse :: Lattice -> Lattice -> Position -> Position -> Lattice
--fuse l1 l2 p1 p2 =
--  let l1' = unpackLattice l1 in
--  let l2' = unpackLattice l2 in
--
--  packLattice . map f $ l1' <> l2'
--
--  where
--    f (x, p, es) = if p == p1 then
--                     (x, p, p2:es)
--                   else
--                     (x, p, es)
--
--unfuse :: Lattice -> Position -> Position -> (Lattice, Lattice)
--unfuse = undefined
--
--footprint :: Lattice -> [Position]
--footprint (Lattice (g, vf, _)) = fmap (view _2 . vf) (vertices g)
--
--absoluteLattice :: Placement -> Lattice -> Lattice
--absoluteLattice (Placement anchor r) =
--  packLattice . fmap (offset . rotate) . unpackLattice
--
--  where
--    rotate :: (Element, Position, [Position]) -> (Element, Position, [Position])
--    rotate (e, p, edges) =
--      (e, rotateHexAroundOrigin r p, fmap (rotateHexAroundOrigin r) edges)
--
--    offset :: (Element, Position, [Position]) -> (Element, Position, [Position])
--    offset (e, p, edges) =
--      (e, anchor <> p, fmap (anchor <>) edges)
--
--unpackLattice :: Lattice -> [(Element, Position, [Position])]
--unpackLattice (Lattice (g, vf, _)) = fmap vf (vertices g)
--
--rotateHexAroundOrigin :: Radians -> Position -> Position
--rotateHexAroundOrigin r p =
--  let Position x y = p in
--  let (Radians r') = r in
--  let (px, py) = hexToPixel (x, y) in
--  let px' = px * cos r' - py * sin r' in
--  let py' = py * cos r' + px * sin r' in
--  let (x', y') = pixelToHex (px', py') in
--
--  Position x' y'
--
