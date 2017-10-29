module Lib where

import Data.List (find)
import Debug.Trace

data RadialDirection = RRight | RLeft deriving (Show)

type Position = (Integer, Integer)
type Rotation = (Int, Int) -- (From, Target)
nullRotation = (360, 360)

data Trigger = TimeTrigger Integer deriving (Show)

data Action = Rotate RadialDirection | ClawClose | ClawOpen deriving (Show)

type Program = [(Trigger, [Action])]

data Grabber = Grabber {
    program :: Program
  , closed :: Bool
  , contents :: Maybe (Lattice Element)
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

type PlacedPiece = (Position, Rotation, Piece)

data Board = Board {
    clock :: Integer
  , sinceLastUpdate :: Double
  , pieces :: [PlacedPiece]
  --tracks :: [Track]
} deriving (Show)

placePiece b o p = (b, o, p)

hexToPixel :: (Integer, Integer) -> (Float, Float)
hexToPixel (q, r) =
  (x, y)
  where
    x = sqrt 3 * (fromInteger q + fromInteger r / 2.0)
    y = 3 / 2 * fromInteger r

pixelToHex :: (Float, Float) -> (Integer, Integer)
pixelToHex (x, y) =
  hexRound (q, r)
  where
    q = (x * sqrt(3)/3 - y / 3)
    r = y * 2/3

cubeRound :: (Float, Float, Float) -> (Integer, Integer, Integer)
cubeRound (x, y, z) =
    if x_diff > y_diff && x_diff > z_diff then
      (-ry-rz, ry, rz)
    else if y_diff > z_diff then
      (rx, -rx-rz, rz)
    else
      (rx, ry, -rx-ry)
  where
    rx = (round x) :: Integer
    ry = (round y) :: Integer
    rz = (round z) :: Integer

    x_diff = abs (fromIntegral rx - x)
    y_diff = abs (fromIntegral ry - y)
    z_diff = abs (fromIntegral rz - z)

axialToCube (q, r) = (q, r, -q-r)
cubeToAxial (x, y, z) = (x, z)

hexRound :: (Float, Float) -> (Integer, Integer)
hexRound = cubeToAxial . cubeRound . axialToCube

buildBoard = Board {
  --tracks = [],
  clock = 0,
  sinceLastUpdate = 0,
  pieces = [placePiece (0, 0) nullRotation (GrabberPiece $ Grabber { program =
    [ (TimeTrigger 0, [
      ClawClose
    , Rotate RRight
    , ClawOpen
    , Rotate RLeft
    ])]
    , closed = False
    , contents = Nothing
    }),
    placePiece (1, 0) nullRotation (ReagentPiece $ Reagent { rlayout =
      Lattice [((0, 0), Fire)]}),
    placePiece (1, -1) nullRotation (ProductPiece $ Product { playout =
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
--rotateAxialHex :: RadialDirection -> Position -> Position -> Position
--rotateAxialHex RLeft (bq, br) (oq, or) = (oq', or')
--  where
--    (nq, nr) = (oq - bq, or - br)
--    (x, y, z) = (nq, -nq - nr, nr)
--    (x', y', z') = (-z, -x, -y)
--    (oq', or') = (x' + bq, z' + br)
--rotateAxialHex RRight (bq, br) (oq, or) = (oq', or')
--  where
--    (nq, nr) = (oq - bq, or - br)
--    (x, y, z) = (nq, -nq - nr, nr)
--    (x', y', z') = (-y, -z, -x)
--    (oq', or') = (x' + bq, z' + br)

rotateDegrees :: RadialDirection -> Int -> Int
rotateDegrees RLeft x = x + 60 `mod` 360
rotateDegrees RRight x = (x - 60 + 360) `mod` 360

-- TODO: Type signature here sucks
-- TODO: Verify o == o' at this point
applyAction :: Action -> PlacedPiece -> PlacedPiece
applyAction (Rotate d) (b, (o, o'), p) = (b, (o', rotateDegrees d o'), p)
applyAction ClawClose (b, (o, o'), GrabberPiece g@Grabber { closed = False }) =
  (b, (o', o'), GrabberPiece g { closed = True }) 

-- TODO: Place contents on board
applyAction ClawOpen (b, (o, o'), GrabberPiece g@Grabber { closed = True }) =
  (b, (o', o'), GrabberPiece g { closed = False, contents = Nothing }) 
applyAction _ x = x
  


stepBoard b = b
  { clock = t
  , pieces = map (stepPiece (clock b)) (pieces b)
  }
  where
    t = ((clock b) + 1) `mod` fromIntegral maxProgramLength
    maxProgramLength = maximum $ (map extractProgramLength (pieces b)) -- TODO avoid unsafe maximum
    extractProgramLength (_, _, GrabberPiece Grabber { program = prg }) =
      maximum $ map (\(TimeTrigger t, as) -> t + fromIntegral (length as)) prg
    extractProgramLength _ = 0

--someFunc :: IO ()
--someFunc = do
--  putStrLn $ show (buildBoard)
--  putStrLn $ show (stepBoard buildBoard)
--  putStrLn $ show (stepBoard . stepBoard $ buildBoard)
