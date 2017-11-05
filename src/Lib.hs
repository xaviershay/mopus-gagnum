{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Hex

import Data.List (find)
import Data.Maybe (mapMaybe)
import Debug.Trace
import Control.Monad.State
import Control.Monad.Identity
import Control.Lens hiding (element)

data RadialDirection = RRight | RLeft deriving (Show, Eq)

type Position = (Integer, Integer)
type Rotation = Int -- (From, Target)
nullRotation = 360

newtype Trigger = TimeTrigger Integer deriving (Show, Eq)

data Action = Rotate RadialDirection | ClawClose | ClawOpen deriving (Show, Eq)

type Program = [(Trigger, [Action])]

data Element = Fire | Water | Earth | Air deriving (Show, Eq)

newtype Lattice a = Lattice [(Position, a)] deriving (Show, Eq) -- TODO: Add bonds

data Grabber = Grabber {
    _program :: Program
  , _closed :: Bool
  , _contents :: Maybe (Lattice Element)
} deriving (Show, Eq)
makeLenses ''Grabber

-- TODO: combine Reagent / Product definitions somehow?
newtype Reagent = Reagent {
  rlayout :: Lattice Element
} deriving (Show, Eq)

newtype Product = Product {
  playout :: Lattice Element
} deriving (Show, Eq)

data Piece =
    GrabberPiece Grabber
  | ReagentPiece Reagent
  | ProductPiece Product
  | LatticePiece (Lattice Element)
  deriving (Show, Eq)

type PiecePosition = ((Position, Position), (Rotation, Rotation))
type GrabTarget = (Position, Position, Piece)

data Board = Board {
    _clock :: Integer
  , _sinceLastUpdate :: Double
  , _grid :: [(PiecePosition, Piece)]
} deriving (Show)
makeLenses ''Board

type EvalBoard a = StateT Board Identity a

placePiece b o p = (((b, b), (o, o)), p)

dropContents :: EvalBoard ()
dropContents = do
  g <- use grid

  -- for each grabber with contents, move that lattice back on to the board

  forM_ g f

  where
    f :: (PiecePosition, Piece) -> EvalBoard ()
    f (pos, GrabberPiece grabber@Grabber { _contents = Just lattice }) = do
      let pos' = clawLocation (pos, grabber)

      addPiece pos' (LatticePiece lattice)
      replaceGrabber (pos, grabber) (pos, grabber & contents .~ Nothing)

    f _ = return ()

-- TODO: Needs to account for rotation
addPiece :: Position -> Piece -> EvalBoard ()
addPiece pos piece = grid %= (:) (((pos, pos), (nullRotation, nullRotation)), piece)

stepGrabbers :: EvalBoard ()
stepGrabbers = do
  g <- use grid

  forM_ (mapMaybe f g) stepGrabber

  where
    f (pos, GrabberPiece grabber) = Just (pos, grabber)
    f _                           = Nothing

stepGrabber :: (PiecePosition, Grabber) -> EvalBoard ()
stepGrabber x@(_, grabber) = do
  t <- use clock

  case find (matchTimeTrigger t) (grabber ^. program) of
    Nothing -> return ()
    Just (TimeTrigger t', as) -> applyAction' x (as !! fromIntegral (t - t'))

currentPos :: PiecePosition -> Position
currentPos ((_, x), _) = x

currentRotation :: PiecePosition -> Rotation
currentRotation (_, (_, x)) = x

clawLocation :: (PiecePosition, Grabber) -> Position
clawLocation (p, _) = pixelToHex $ hexToPixel (currentPos p)
  & _1 %~ ((cos o' * sqrt 3) +)
  & _2 %~ ((sin o' * sqrt 3) +)

  where
    o' :: Float
    o' = toRadians $ currentRotation p

pieceAt :: Position -> EvalBoard (Maybe (PiecePosition, Piece))
pieceAt location = do
  g <- use grid

  return $ find (\(p, _) -> location == currentPos p) g

applyAction' :: (PiecePosition, Grabber) -> Action -> EvalBoard ()
applyAction' old@(pos, grabber) ClawClose = do
  let grabber' = grabber & closed .~ True

  replaceGrabber old (stayStill pos, grabber')

applyAction' old@(pos, grabber) ClawOpen = do
  let grabber' = grabber & closed .~ False

  replaceGrabber old (stayStill pos, grabber')

applyAction' old@(pos, grabber) (Rotate direction) = do
  -- Need to pick up the lattice only when moving. Multiple claws are allowed
  -- to grab the same piece if it is static!
  -- TODO: Needs to account for a) where lattice is being picked up, b)
  -- rotation of lattice.
  lattice <- pieceAt (clawLocation old)
  let pos' = rotateDegrees direction pos

  case (grabber ^. closed, lattice) of
    (True, Just piece@(_, LatticePiece xs)) -> do
      removePiece piece
      replaceGrabber old (pos', grabber & contents .~ Just xs)
    _ ->
      replaceGrabber old (pos', grabber)

stayStill ((_, p'), (_, o')) = ((p', p'), (o', o'))

rotateDegrees :: RadialDirection -> PiecePosition -> PiecePosition
rotateDegrees direction ((_, p'), (_, o'))  = ((p', p'), (o', o''))
  where
    o'' = o' + toDegrees direction + 360 `mod` 360
    toDegrees RRight = -60
    toDegrees RLeft = 60

replaceGrabber :: (PiecePosition, Grabber) -> (PiecePosition, Grabber) -> EvalBoard ()
replaceGrabber (op, og) (np, ng) = do
  let old' = (op, GrabberPiece og)
  let new' = (np, GrabberPiece ng)

  grid . traverse %= (\x -> if x == old' then new' else x)

removePiece :: (PiecePosition, Piece) -> EvalBoard ()
removePiece p = grid %= filter (p /=)

advanceClock :: EvalBoard ()
advanceClock = do
  t <- use clock
  g <- use grid

  -- TODO avoid unsafe maximum
  let maxProgramLength = maximum $ map extractProgramLength g

  let t' = (t + 1) `mod` fromIntegral maxProgramLength

  clock .= t'

  where
    extractProgramLength (_, GrabberPiece grabber) =
      maximum $
        map (\(TimeTrigger t, as) -> t + fromIntegral (length as))
        (grabber ^. program)
    extractProgramLength _ = 0

stepBoard :: Board -> Board
stepBoard b = runIdentity $ execStateT (dropContents >> stepGrabbers >> advanceClock) b

matchTimeTrigger :: Integer -> (Trigger, [Action]) -> Bool
matchTimeTrigger t (TimeTrigger t', as) = t' <= t && t < (t' + fromIntegral (length as))

toRadians d = fromIntegral d * pi / 180
moveRightProgram = [ (TimeTrigger 0,
                     [ ClawClose
                     , Rotate RRight
                     , ClawOpen
                     , Rotate RLeft
                     ])]

buildBoard = Board {
  _clock = 0,
  _sinceLastUpdate = 0,
  _grid = [
      placePiece (0, 0) nullRotation (GrabberPiece Grabber {
          _program = moveRightProgram
        , _closed = False
        , _contents = Nothing
      })
    , placePiece (2, 0) 240 (GrabberPiece Grabber {
          _program = moveRightProgram
        , _closed = False
        , _contents = Nothing
      })
    , placePiece (0, 2) 120 (GrabberPiece Grabber {
          _program = moveRightProgram
        , _closed = False
        , _contents = Nothing
      })
    , placePiece (1, 0) nullRotation (LatticePiece (Lattice [((0, 0), Fire)]))
    --, placePiece (1, 0) nullRotation (ReagentPiece Reagent { rlayout =
    --  Lattice [((0, 0), Fire)]})
    --, placePiece (0, 1) nullRotation (ProductPiece Product { playout =
    --  Lattice [((0, 0), Fire)]})
  ]
}
