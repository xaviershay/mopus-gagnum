module Lattice where

import Hex
import Types

import           Control.Lens
import qualified Data.Graph.Inductive       as G
import qualified Data.Graph.Inductive.Query as G
import           Data.List                  ((\\))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))

type NodeData = (Element, Position)
type EdgeData = ()
type LatticeGraph = G.Gr NodeData EdgeData
newtype Lattice = Lattice LatticeGraph

type UnpackedNode = (Element, Position, [Position])
type UnpackedLattice = [UnpackedNode]

--instance Show Lattice where
--  show = (<>) "packLattice " . show . unpackLattice
instance Show Lattice where
  show (Lattice g) = show $ G.edges g

-- TODO: should do undirected comparison, rather than directed
instance Eq Lattice where
  g1 == g2 = unpackLattice g1 == unpackLattice g2

-- TODO: Error on graphs with unconnected components?
packLattice :: UnpackedLattice -> Lattice
packLattice xs = Lattice $ G.mkGraph nodes edges
  where
    nodeIndices = G.newNodes (length xs) (G.empty :: LatticeGraph)
    positionIndices = M.fromList $ zip (fmap (^. _2) xs) nodeIndices
    nodes = zip nodeIndices (fmap (\(a, b, _) -> (a, b)) xs)
    edges = concatMap (\(_, p, es) -> fmap (\e -> (nodeIndex p, nodeIndex e, ())) es) xs
    nodeIndex p = fromJust $ M.lookup p positionIndices

unpackLattice :: Lattice -> UnpackedLattice
unpackLattice (Lattice g) = G.ufold f mempty g
  where
    f :: G.Context NodeData EdgeData -> UnpackedLattice -> UnpackedLattice
    f (_, _, (a, b), es) xs = (a, b, fmap (snd . fromJust . G.lab g . snd) es):xs

-- Given two non-overlapping absolute lattices and a position ("fuse point") on
-- each lattice, create a single lattice connected by the two positions.
--
-- This implementation likely isn't particularly efficient, it iterates over
-- graphs multiple times.
--
-- Pre-condition: both lattices are absolutely positioned, and share no nodes.
-- Pre-condition: each fuse point refers to a node on respective lattices.
-- Post-condition: returned lattice is a single connected component.
fuse :: Lattice -> Lattice -> Position -> Position -> Lattice
fuse (Lattice g1) (Lattice g2) f1 f2 =
  -- g2 will be merged into g1
  let nodesToAdd = G.labNodes g2 in
  let edgesToAdd = G.labEdges g2 in

  -- A function to map nodes in g2 to new nodes in g1 domain
  let nodeF n = fromJust . M.lookup n . M.fromList $
                  zip (fmap fst nodesToAdd)
                      (G.newNodes (length nodesToAdd) g1) in

  -- Insert nodes and egdes from g2 to g1, remapping node indices to new
  -- domain.  Recall that LNode = (Node, a) and LEdge = (Node, Node, b).
  let merged =
        G.insEdges (fmap (over _1 nodeF . over _2 nodeF) edgesToAdd) $
        G.insNodes (fmap (over _1 nodeF) nodesToAdd) g1 in

  -- A reverse index function to map positions to node indices.
  let posF p = fromJust $ M.lookup p
        (M.fromList . fmap invertLabNode . G.labNodes $ merged) in

  -- Add a new edge between the two fuse points.
  Lattice . G.insEdge (mkEdge (posF f1) (posF f2)) $ merged

  where
    invertLabNode (n, (e, p)) = (p, n)
    mkEdge a b = (a, b, ())


--unfuse :: Lattice -> Position -> Position -> (Lattice, Lattice)
--unfuse = undefined
--
footprint :: Lattice -> [Position]
footprint = map (snd . snd) . G.labNodes . toInductiveGraph

absoluteLattice :: Placement -> Lattice -> Lattice
absoluteLattice (Placement anchor r) (Lattice g) =
  Lattice $ G.nmap transformNode g

  where
    transformNode = over position (offset . rotate)

    rotate :: Position -> Position
    rotate = rotateHexAroundOrigin r

    offset :: Position -> Position
    offset = (<>) anchor

rotateHexAroundOrigin :: Radians -> Position -> Position
rotateHexAroundOrigin r p =
  let Position x y = p in
  let (Radians r') = r in
  let (px, py) = hexToPixel (x, y) in
  let px' = px * cos r' - py * sin r' in
  let py' = py * cos r' + px * sin r' in
  let (x', y') = pixelToHex (px', py') in

  Position x' y'

-- PRIVATE HELPER METHODS
toInductiveGraph :: Lattice -> LatticeGraph
toInductiveGraph (Lattice g) = g

-- Takes a graph with >= 1 components and splits into one or more graphs with
-- exactly one component each. Used for quickcheck shrinking.
splitGraph :: LatticeGraph -> [LatticeGraph]
splitGraph g = f (G.nodes g)
  where
    f [] = []
    f (n:ns) =
      let nodesInCluster = G.bfs n (G.undir g) in
      G.subgraph nodesInCluster g:f (ns \\ nodesInCluster)

-- Lens selectors. Element unused, so commented out.
--element = _1
position = _2
