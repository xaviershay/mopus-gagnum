module Lib where

import Data.List (find)
import Data.Maybe (mapMaybe)
import Debug.Trace
import Control.Monad.State
import Control.Monad.Identity

type EvalBoard a = StateT Board' Identity a

data RadialDirection = RRight | RLeft deriving (Show, Eq)

type Position = (Integer, Integer)
type Rotation = Int -- (From, Target)
nullRotation = 360

data Trigger = TimeTrigger Integer deriving (Show, Eq)

data Action = Rotate RadialDirection | ClawClose | ClawOpen deriving (Show, Eq)

type Program = [(Trigger, [Action])]

data Grabber = Grabber {
    program :: Program
  , closed :: Bool
  , contents :: Maybe (Lattice Element)
} deriving (Show, Eq)

data Element = Fire | Water | Earth | Air deriving (Show, Eq)

data Lattice a = Lattice [(Position, a)] deriving (Show, Eq) -- TODO: Add bonds

-- TODO: combine Reagent / Product definitions somehow?
data Reagent = Reagent {
  rlayout :: Lattice Element
} deriving (Show, Eq)

data Product = Product {
  playout :: Lattice Element
} deriving (Show, Eq)

data Piece = GrabberPiece Grabber | ReagentPiece Reagent | ProductPiece Product deriving (Show, Eq)

type PlacedPiece = (Position, Rotation, Piece)

type PiecePosition = ((Position, Position), (Rotation, Rotation))
type GrabTarget = (Position, Position, Piece)

data Board' = Board' {
    _clock :: Integer
  , _sinceLastUpdate :: Double
  , _map :: [(PiecePosition, Piece)]
} deriving (Show)

placePiece b o p = (((b, b), (o, o)), p)

stepGrabbers :: EvalBoard ()
stepGrabbers = do
  brd <- get

  forM_ (mapMaybe f $ _map brd) stepGrabber

  where
    f (pos, GrabberPiece grabber) = Just (pos, grabber)
    f _                           = Nothing

stepGrabber :: (PiecePosition, Grabber) -> EvalBoard ()
stepGrabber x@(_, grabber) = do
  brd <- get
  let t = _clock brd

  case find (matchTimeTrigger t) (program grabber) of
    Nothing -> return ()
    Just (TimeTrigger t', as) -> applyAction' x (as !! fromIntegral (t - t'))

applyAction' :: (PiecePosition, Grabber) -> Action -> EvalBoard ()
applyAction' old@(pos, grabber) ClawClose = do
  let grabber' = grabber { closed = True }
  
  replaceGrabber old (stayStill pos, grabber')

applyAction' old@(pos, grabber) ClawOpen = do
  let grabber' = grabber { closed = False }
  
  replaceGrabber old (stayStill pos, grabber')

applyAction' old@(pos, grabber) (Rotate direction) = do
  let pos' = rotateDegrees direction pos
  replaceGrabber old (pos', grabber)

stayStill ((_, p'), (_, o')) = ((p', p'), (o', o'))

rotateDegrees :: RadialDirection -> PiecePosition -> PiecePosition
rotateDegrees direction ((_, p'), (_, o'))  = ((p', p'), (o', o''))
  where
    o'' = o' + toDegrees direction + 360 `mod` 360
    toDegrees RRight = (-60)
    toDegrees RLeft = 60

replaceGrabber :: (PiecePosition, Grabber) -> (PiecePosition, Grabber) -> EvalBoard ()
replaceGrabber (op, og) (np, ng) = do
  brd <- get
  let old' = (op, GrabberPiece og)
  let new' = (np, GrabberPiece ng)

  put $ brd { _map = map (\x -> if x == old' then new' else x) (_map brd) }

advanceClock :: EvalBoard ()
advanceClock = do
  brd <- get
  
  let maxProgramLength = maximum $ (map extractProgramLength (_map brd)) -- TODO avoid unsafe maximum

  let t = _clock brd
  let t' = (t + 1) `mod` fromIntegral maxProgramLength

  put $ brd { _clock = t' }

  where
    extractProgramLength (_, GrabberPiece Grabber { program = prg }) =
      maximum $ map (\(TimeTrigger t, as) -> t + fromIntegral (length as)) prg
    extractProgramLength _ = 0

stepBoard :: Board' -> Board'
stepBoard b = runIdentity $ execStateT (stepGrabbers >> advanceClock) b

hexToPixel :: (Integer, Integer) -> (Float, Float)
hexToPixel (q, r) =
  (x, y)
  where
    x = sqrt 3 * (fromInteger q + fromInteger r / 2.0)
    y = -3 / 2 * fromInteger r

pixelToHex :: (Float, Float) -> (Integer, Integer)
pixelToHex (x, y) =
  hexRound (q, r)
  where
    q = (x * sqrt(3)/3 - y / (-3))
    r = y * (-2)/3

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

axialToCube (q, r) = (q, -q-r, r)
cubeToAxial (x, y, z) = (x, z)

hexRound :: (Float, Float) -> (Integer, Integer)
hexRound = cubeToAxial . cubeRound . axialToCube

buildBoard = Board' {
  _clock = 0,
  _sinceLastUpdate = 0,
  _map = [placePiece (0, 0) nullRotation (GrabberPiece $ Grabber { program =
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
    placePiece (0, 1) nullRotation (ProductPiece $ Product { playout =
      Lattice [((0, 0), Fire)]})
  ]
}

matchTimeTrigger :: Integer -> (Trigger, [Action]) -> Bool
matchTimeTrigger t (TimeTrigger t', as) = t' <= t && t < (t' + (fromIntegral $ length as))

toRadians d = fromIntegral d * pi / 180
