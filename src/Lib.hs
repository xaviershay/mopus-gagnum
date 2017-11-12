{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Types
import Hex
import Lattice

import Data.List (find)
import Data.Maybe (mapMaybe, fromJust, catMaybes)
import Data.Fixed (mod')
import qualified Data.Map.Strict as M
import Debug.Trace
import Control.Monad.State
import Control.Monad.Writer hiding (Product)
import Control.Monad.Identity
import Control.Lens hiding (element)

data RadialDirection = RRight | RLeft deriving (Show, Eq)

newtype Trigger = TimeTrigger Integer deriving (Show, Eq)

data Action = Rotate RadialDirection | ClawClose | ClawOpen deriving (Show, Eq)

type Program = [(Trigger, [Action])]

data Grabber = Grabber {
    _program :: Program
  , _closed :: Bool
  , _contents :: Maybe Lattice
} deriving (Show, Eq)
makeLenses ''Grabber

data Piece =
    GrabberPiece Grabber
  | LatticePiece Lattice
  deriving (Show, Eq)

data GroundPiece =
    Producer Lattice
  | Consumer Lattice
  deriving (Show, Eq)

type GrabTarget = (Position, Position, Piece)

type Key = Placement

data Transition =
    ToRotation Radians
  | ToClaw Bool
  | Consume
  | Produce
  deriving (Show)

-- TODO: Switch to Seq for better concatenation performance
type TransitionList = [(Key, Transition)]
type Delta = Double

data Board = Board {
    _clock :: Integer
  , _sinceLastUpdate :: Delta
  , _grid :: [(Placement, Piece)]
  , _ground :: M.Map Position (Placement, GroundPiece)
  , _transitions :: TransitionList
} deriving (Show)
makeLenses ''Board

type EvalBoard a = WriterT TransitionList (StateT Board Identity) a

placePiece :: (Integer, Integer) -> Radians -> Piece -> (Placement, Piece)
placePiece (x, y) o p = (Placement (Position x y) o, p)

-- TODO: Make a lense instead
groundPieces :: Board -> [(Placement, GroundPiece)]
groundPieces = map snd . M.toList . _ground

groundPiecesWithTransitions :: Board -> [((Placement, GroundPiece), Maybe Transition)]
groundPiecesWithTransitions b = map f (M.toList $ b ^. ground)
  where
    f :: (Position, (Placement, GroundPiece)) -> ((Placement, GroundPiece), Maybe Transition)
    f (_, x@(placement, piece)) = (x, snd <$> find (\(k, _) -> k == placement) (b ^. transitions))

piecesWithTransitions :: Board -> [((Placement, Piece), Maybe Transition)]
piecesWithTransitions b = map f (b ^. grid)
  where
    f :: (Placement, Piece) -> ((Placement, Piece), Maybe Transition)
    f x@(placement, piece) = (x, snd <$> find (\(k, _) -> k == placement) (b ^. transitions))

applyTransition :: Transition -> Delta -> (Placement, Piece) -> (Placement, Maybe Piece)
applyTransition (ToClaw c) _ (placement, GrabberPiece grabber) =
  (placement, Just $ GrabberPiece $ grabber & closed .~ c)
applyTransition (ToRotation r') delta (Placement p r, piece) =
  (Placement p $ r + (r' - r) * (Radians $ realToFrac delta), Just piece)
applyTransition Consume delta (pl, x) = (pl, if delta < 1 then Just x else Nothing)
applyTransition Produce delta (pl, x) = (pl, Just x) -- TODO: Add "collidable" property

dropContents :: EvalBoard ()
dropContents = do
  ts <- use transitions

  f ts

  g <- use grid
  forM_ g emptyGrabbers

  transitions .= mempty

  where
    f :: TransitionList -> EvalBoard ()
    f [] = return ()
    f ((k, t):ts) = do
      oldPiece <- pieceAt k -- TODO: Avoid unsafe fromJust
      let newPiece = applyTransition t 1.0 (k, fromJust oldPiece)

      removePiece k
      case newPiece of
        (placement, Just piece) -> addPiece (placement, piece)
        _ -> return ()

      f ts

    emptyGrabbers :: (Placement, Piece) -> EvalBoard ()
    emptyGrabbers (pos, GrabberPiece grabber) =
      case grabber ^. contents of
        Nothing -> return ()
        Just lattice -> do
          let clawPos = clawLocation (pos, grabber)

          addPiece (clawPos, LatticePiece lattice)
          updateGrabber pos (grabber & contents .~ Nothing)
    emptyGrabbers _ = return ()

extractLattices :: EvalBoard [(Placement, Lattice)]
extractLattices = do
  g <- use grid

  return (mapMaybe f g)

  where
    f (p, LatticePiece l) = Just (p, l)
    f _ = Nothing

extractProducers :: EvalBoard [(Placement, Lattice)]
extractProducers = do
  g <- use ground

  return (mapMaybe f (M.toList g))

  where
    f :: (Position, (Placement, GroundPiece)) -> Maybe (Placement, Lattice)
    f (_, (p, Producer l)) = Just (p, l)
    f _ = Nothing

groundAt :: Placement -> EvalBoard (Maybe (Placement, GroundPiece))
groundAt (Placement p _) = do
  g <- use ground
  return $ M.lookup p g

consumeAndProduce :: EvalBoard ()
consumeAndProduce = do
  lattices <- extractLattices
  forM_ lattices $ \(p1, l1) -> do
    x <- groundAt p1

    case x of
      Just (p2, Consumer l2) ->
        when (absoluteLattice p1 l1 == absoluteLattice p2 l2)
             (addTransition p1 Consume)
      _ -> return ()

  producers <- extractProducers

  forM_ producers $ \(rootPos, lattice) -> do
    let absolutePositions = map (\p -> rootPos <> Placement p 0)
                              (footprint lattice)

    existing <- mapM pieceAt absolutePositions

    when (null $ catMaybes existing) $ do
      addTransition rootPos Produce
      addPiece (rootPos, LatticePiece lattice)

addPiece :: (Placement, Piece) -> EvalBoard ()
addPiece x = grid %= (:) x

stepGrabbers :: EvalBoard ()
stepGrabbers = do
  g <- use grid

  forM_ (mapMaybe f g) stepGrabber

  where
    f (pos, GrabberPiece grabber) = Just (pos, grabber)
    f _                           = Nothing

stepGrabber :: (Placement, Grabber) -> EvalBoard ()
stepGrabber x@(_, grabber) = do
  t <- use clock

  case find (matchTimeTrigger t) (grabber ^. program) of
    Nothing -> return ()
    Just (TimeTrigger t', as) -> applyAction' x (as !! fromIntegral (t - t'))

currentPos :: Placement -> Position
currentPos (Placement x _) = x

currentRotation :: Placement -> Radians
currentRotation (Placement _ x) = x

clawLocation :: (Placement, Grabber) -> Placement
clawLocation (p, _) =
  let Position x y = currentPos p in
  let (x', y') = pixelToHex $ hexToPixel (x, y)
                & _1 %~ ((cos o * sqrt 3) +)
                & _2 %~ ((sin o * sqrt 3) +) in

  Placement (mkPos x' y') (currentRotation p) -- TODO: Allow for claw rotation

  where
    (Radians o) = currentRotation p

pieceAt :: Placement -> EvalBoard (Maybe Piece)
pieceAt location = do
  g <- use grid

  return $ snd <$> find (\(p, _) -> location == p) g

applyAction' :: (Placement, Grabber) -> Action -> EvalBoard ()
applyAction' (pos, _) ClawClose = addTransition pos (ToClaw True)
applyAction' (pos, _) ClawOpen  = addTransition pos (ToClaw False)

applyAction' old@(pos, grabber) (Rotate direction) = do
  -- Need to pick up the lattice only when moving. Multiple claws are allowed
  -- to grab the same piece if it is static!
  -- TODO: Needs to account for a) where lattice is being picked up, b)
  -- rotation of lattice.
  let latticePos = clawLocation old
  lattice <- pieceAt latticePos
  let (Placement _ newRotation) = rotateDirection direction pos

  case (grabber ^. closed, lattice) of
    (True, Just (LatticePiece xs)) -> do
      removePiece latticePos
      updateGrabber pos (grabber & contents .~ Just xs)
      addTransition pos (ToRotation newRotation)
    _ ->
      addTransition pos (ToRotation newRotation)

addTransition :: Placement -> Transition -> EvalBoard ()
addTransition k t = tell [(k, t)]

rotateDirection :: RadialDirection -> Placement -> Placement
rotateDirection direction (Placement p o) = Placement p o'
  where
    o' = o + f direction + (2 * pi) `mod'` (2 * pi)
    f RRight = hexRotation (-1)
    f RLeft  = hexRotation 1

updateGrabber :: Key -> Grabber -> EvalBoard ()
updateGrabber key new =
  grid . traverse %= (\(k, old) -> (k, if k == key then GrabberPiece new else old))

removePiece :: Key -> EvalBoard ()
removePiece (Placement key _) = grid %= filter (\(Placement k _, _) -> k /= key)

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
stepBoard b =
  let t = execWriterT (dropContents >> consumeAndProduce >> stepGrabbers >> advanceClock) :: StateT Board Identity TransitionList in
  let s = runStateT t b :: Identity (TransitionList, Board) in
  let (ts, board) = runIdentity s in

  board & transitions .~ ts

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
  _transitions = mempty,
  _ground = M.fromList
    [ (mkPos 0 1, (Placement (mkPos 0 1) (hexRotation 5), Consumer (packLattice
      [(Fire, mkPos 0 0, [mkPos 1 0])
      ,(Fire, mkPos 1 0, [])
      ])))
    , (mkPos 1 0, (Placement (mkPos 1 0) (hexRotation 0), Producer (packLattice
      [(Fire, mkPos 0 0, [mkPos 1 0])
      ,(Fire, mkPos 1 0, [])
      ])))
    ],
  _grid = [
      placePiece (0, 0) (hexRotation 0) (GrabberPiece Grabber {
          _program = moveRightProgram
        , _closed = False
        , _contents = Nothing
      })
--    , placePiece (2, 0) (hexRotation 4) (GrabberPiece Grabber {
--          _program = moveRightProgram
--        , _closed = False
--        , _contents = Nothing
--      })
--    , placePiece (0, 2) (hexRotation 2) (GrabberPiece Grabber {
--          _program = moveRightProgram
--        , _closed = False
--        , _contents = Nothing
--      })
--    , placePiece (1, 0) (hexRotation 0) (LatticePiece (Lattice [((0, 0), Fire)]))
--    , placePiece (-2, -1) (hexRotation 5) (LatticePiece $ packLattice
--      [(Fire, mkPos 0 0, [mkPos 1 0])
--      ,(Fire, mkPos 1 0, [])
--      ])
  ]
}
