module Lib where

import Data.List (find)
import Debug.Trace

data RadialDirection = RRight | RLeft deriving (Show)

type Position = (Integer, Integer)

data Trigger = TimeTrigger Integer deriving (Show)

data Action = Rotate RadialDirection | ClawClose | ClawOpen deriving (Show)

type Program = [(Trigger, [Action])]

data Grabber = Grabber {
  program :: Program
} deriving (Show)

data Element = Fire | Water | Earth | Air deriving (Show)

data Lattice a = Lattice [(Position, a)] deriving (Show) -- TODO: Add bonds

-- TODO: combine Reagent / Product definitions somehow?
data Reagent = Reagent {
  rlayout :: Lattice Element
} deriving (Show)

data Product = Product {
  playout :: Lattice Element
} deriving (Show)

data Piece = GrabberPiece Grabber | ReagentPiece Reagent | ProductPiece Product deriving (Show)

type PlacedPiece = (Position, Position, Piece)

data Board = Board {
    clock :: Integer
  , pieces :: [PlacedPiece]
  --tracks :: [Track]
} deriving (Show)

placePiece b o p = (b, o, p)

buildBoard = Board {
  --tracks = [],
  clock = 0,
  pieces = [placePiece (0, 0) (1, 0) (GrabberPiece $ Grabber { program =
    [ (TimeTrigger 0, [
    -- ClawClose
      Rotate RRight
    --, ClawOpen
    , Rotate RLeft
    ])]}),
    placePiece (1, 0) (0, 0) (ReagentPiece $ Reagent { rlayout =
      Lattice [((0, 0), Fire)]}),
    placePiece (1, -1) (0, 0) (ProductPiece $ Product { playout =
      Lattice [((0, 0), Fire)]})
  ]
}

matchTimeTrigger :: Integer -> (Trigger, [Action]) -> Bool
matchTimeTrigger t (TimeTrigger t', as) = t' <= t && t < (t' + (fromIntegral $ length as))

stepPiece :: Integer -> PlacedPiece -> PlacedPiece
stepPiece t p@(b, o, GrabberPiece g) =
  case find (matchTimeTrigger t) (program g) of
    Nothing -> p
    Just (TimeTrigger t', as) -> applyAction (as !! fromIntegral (t - t')) p
stepPiece t p = p -- TODO

-- Implementation from https://www.redblobgames.com/grids/hexagons/#rotation
-- with directions switched to match diagram at
-- https://github.com/mhwombat/grid/wiki/Hexagonal-tiles
rotateAxialHex :: RadialDirection -> Position -> Position -> Position
rotateAxialHex RLeft (bq, br) (oq, or) = (oq', or')
  where
    (nq, nr) = (oq - bq, or - br)
    (x, y, z) = (nq, -nq - nr, nr)
    (x', y', z') = (-z, -x, -y)
    (oq', or') = (x' + bq, z' + br)
rotateAxialHex RRight (bq, br) (oq, or) = (oq', or')
  where
    (nq, nr) = (oq - bq, or - br)
    (x, y, z) = (nq, -nq - nr, nr)
    (x', y', z') = (-y, -z, -x)
    (oq', or') = (x' + bq, z' + br)

applyAction :: Action -> PlacedPiece -> PlacedPiece
applyAction (Rotate d) (b, o, p) = (b, rotateAxialHex d b o, p)

stepBoard b = b
  { clock = t
  , pieces = map (stepPiece (clock b)) (pieces b)
  }
  where
    t = ((clock b) + 1) `mod` (trace (show maxProgramLength) $ fromIntegral maxProgramLength)
    maxProgramLength = maximum $ trace (show $ map extractProgramLength (pieces b)) (map extractProgramLength (pieces b)) -- TODO avoid unsafe maximum
    extractProgramLength (_, _, GrabberPiece Grabber { program = prg }) =
      maximum $ map (\(TimeTrigger t, as) -> t + fromIntegral (length as)) prg
    extractProgramLength _ = 0

someFunc :: IO ()
someFunc = do
  putStrLn $ show (buildBoard)
  putStrLn $ show (stepBoard buildBoard)
  putStrLn $ show (stepBoard . stepBoard $ buildBoard)
