import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import Debug.Trace

import Types
import Lattice
import Hex

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup ""
  [ testGroup "Lattices" latticeTests
  ]

latticeTests :: [TestTree]
latticeTests =
  [ testGroup "rotateHexAroundOrigin"
    [ testProperty "identity" $ \position ->
        position == rotateHexAroundOrigin (Radians 0) position
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
