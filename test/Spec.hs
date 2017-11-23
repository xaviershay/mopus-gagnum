import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import System.Random

import Debug.Trace

import Data.Graph (components)
import Data.List (intersect, delete)

import Types
import Lattice
import Hex

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup ""
  [ testGroup "Lattices" latticeTests
  ]

instance Arbitrary Element where
  arbitrary = elements [Fire, Water, Earth, Air]

instance Arbitrary Lattice where
  shrink = map packLattice . shrink' . unpackLattice
    where
      shrink' []  = [] -- Invalid lattice, but including for completeness
      shrink' [x] = []
      shrink' xs =
        let edges = concatMap (\(_, p, es) -> map ((,) p) es) xs in

        let disconnectedGraph = map (removeEdge xs) edges

        where
          removeEdge [] _ = []
          removeEdge (x@(a, p, es):xs) edge@(p', e) =
            if p == p' then
              (a, p, delete e es):xs
            else
              x:removeEdge xs edge

  arbitrary = do
    ps <- arbitrary `suchThat` (not . null)
    connected <- addEdges $ (map (\(p, e) -> (p, e, []))) ps

    return . packLattice $ connected

    where
      addEdges :: [(Element, Position, [Position])] -> Gen [(Element, Position, [Position])]
      addEdges [] = error "addEdges should never be called with empty array"
      addEdges [x] = return [x]
      addEdges ((p, e, _):xs) = do
        existing <- addEdges xs
        toAdd <- sublistOf existing `suchThat` (not . null)

        let es = map (\(_, p, _) -> p) toAdd

        return ((p, e, es):existing)

numComponents = length . components . toGraph

latticeTests :: [TestTree]
latticeTests =
  [ testGroup "rotateHexAroundOrigin"
    [ testProperty "identity" $ \position ->
        position == rotateHexAroundOrigin (Radians 0) position
    ]
  , testGroup "lattice generation"
    [ testProperty "generates a single connected component" $ \l ->
        (length . components . toGraph $ l) == 1
    ]
  , testGroup "fuse"
    [ testCase "simple" $
        let l1 = absoluteLattice (Placement (mkPos 0 0) (hexRotation 0)) $
                   packLattice [ (Fire, mkPos 0 0, []) ] in
        let l2 = absoluteLattice (Placement (mkPos 1 0) (hexRotation 0)) $
                   packLattice [ (Fire, mkPos 0 0, []) ] in

        let expected = packLattice
                        [ (Fire, mkPos 0 0, [mkPos 1 0])
                        , (Fire, mkPos 1 0, [])
                        ] in
        let actual = fuse l1 l2 (mkPos 0 0) (mkPos 1 0) in

        expected @=? actual
    , testProperty "always returns a single connected component" $ \l1 l2 ->
        let ps1 = extractPositions l1 in
        let ps2 = extractPositions l2 in
        let pairs = [(a, b) | a <- ps1, b <- ps2] in

        null (ps1 `intersect` ps2) ==>
          all (\(a, b) -> numComponents (fuse l1 l2 a b) == 1) pairs
    ]
  , testGroup "absoluteFootprint"
    [ testCase "identity" $
        let lattice = packLattice
                        [ (Fire, mkPos 0 0, [mkPos 1 0])
                        , (Fire, mkPos 1 0, [])
                        ] in
        let actual = absoluteLattice (Placement mempty (hexRotation 0)) lattice
          in
        lattice @=? actual
    , testCase "rotation" $
        let lattice = packLattice
                        [ (Fire, mkPos 0 0, [mkPos 1 0])
                        , (Water, mkPos 1 0, [])
                        ] in
        let expected = packLattice
                        [ (Fire, mkPos 0 0, [mkPos 1 (-1)])
                        , (Water, mkPos 1 (-1), [])
                        ] in
        let actual = absoluteLattice (Placement mempty (hexRotation 1)) lattice
          in
        expected @=? actual
    , testCase "position" $
        let lattice = packLattice
                        [ (Fire, mkPos 0 0, [mkPos 1 0])
                        , (Water, mkPos 1 0, [])
                        ] in
        let expected = packLattice
                        [ (Fire, mkPos 1 1, [mkPos 2 1])
                        , (Water, mkPos 2 1, [])
                        ] in
        let actual = absoluteLattice (Placement (Position 1 1) (hexRotation 0)) lattice
          in
        expected @=? actual
--    TODO: Lattices current directed
--    , testCase "different base" $
--        let lattice = packLattice
--                        [ (Fire, mkPos 0 0, [mkPos 1 0])
--                        , (Water, mkPos 1 0, [])
--                        ] in
--        let expected = packLattice
--                        [ (Water, mkPos 1 0, [mkPos 0 0])
--                        , (Fire, mkPos 0 0, [])
--                        ] in
--        let actual = absoluteLattice (Placement mempty 0) lattice
--          in
--        expected @=? actual
     ]
   ]

instance Arbitrary Position where
  arbitrary = Position <$> arbitrary <*> arbitrary
