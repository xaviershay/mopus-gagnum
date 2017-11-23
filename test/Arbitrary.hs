module Arbitrary where

import           Control.Monad         (liftM2)
import qualified Data.Graph.Inductive  as G
import           Data.List             (nub)
import           Test.Tasty.QuickCheck

import Types
import Lattice

instance Arbitrary Element where
  arbitrary = elements [Fire, Water, Earth, Air]

instance Arbitrary Lattice where
  shrink (Lattice g) =
    case G.edges g of
      [] -> []
      (e:es) -> map Lattice (splitGraph $ G.delEdge e g)

  arbitrary = do
    xs <- liftM2 zip
            (nub <$> listOf1 arbitrary :: Gen [Position])
            (infiniteListOf arbitrary :: Gen [Element])

    connected <- addEdges . fmap (\(p, e) -> (e, p, [])) $ xs

    return . packLattice $ connected

    where
      addEdges :: UnpackedLattice -> Gen UnpackedLattice
      addEdges [] = error "addEdges should never be called with empty array"
      addEdges [x] = return [x]
      addEdges ((p, e, _):xs) = do
        existing <- addEdges xs
        toAdd <- sublistOf1 existing

        let es = map (\(_, p, _) -> p) toAdd

        return ((p, e, es):existing)

sublistOf1 :: [a] -> Gen [a]
sublistOf1 xs = sublistOf xs `suchThat` (not . null)

instance Arbitrary Position where
  arbitrary = Position <$> arbitrary <*> arbitrary

