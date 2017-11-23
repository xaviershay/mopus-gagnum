import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import Debug.Trace

import qualified Data.Graph.Inductive as G
import           Data.List            (delete, intersect)

import Types
import Lattice
import Hex
import Arbitrary

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup ""
  [ testGroup "Lattice" latticeTests
  ]

latticeTests :: [TestTree]
latticeTests =
  [ testGroup "Arbitrary instance"
    [ testProperty "generates a single connected component" $ \l ->
        numComponents l == 1
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
    , testProperty "always returns a single connected component" .
      withMaxSuccess 20 $ \l1 l2 ->
        let ps1 = footprint l1 in
        let ps2 = footprint l2 in
        let pairs = [(a, b) | a <- ps1, b <- ps2] in

        null (ps1 `intersect` ps2) ==>
          all (\(a, b) -> numComponents (fuse l1 l2 a b) == 1) pairs
    ]
  , testGroup "packLattice and unpackLattice"
    -- Reduce number of checks because for some reasons this property takes a
    -- while.
    [ testProperty "identity" . withMaxSuccess 20 $ \l ->
        (packLattice . unpackLattice) l == l
    ]
  , testGroup "rotateHexAroundOrigin"
    [ testProperty "identity" $ \position ->
        position == rotateHexAroundOrigin (Radians 0) position
    ]
  ]
  ]

numComponents = length . splitGraph . toInductiveGraph
