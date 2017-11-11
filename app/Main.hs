module Main where

import Lib hiding (Position)
import Hex
import Data.List (find)
import Graphics.UI.GLUT
import Data.IORef ( IORef, newIORef, writeIORef )
import Data.Time.Clock ( getCurrentTime, diffUTCTime, UTCTime )
import Control.Lens hiding (element)
import Control.Monad (forM_, when)
import System.Exit ( exitFailure, exitSuccess )
import Debug.Trace

data State = State
  { board :: IORef Board
  , lastUpdate :: IORef UTCTime
  , pan :: IORef (Double, Double)
  , lastPanPos :: IORef (Maybe Position)
  }

mkState :: IO State
mkState = do
  t <- newIORef =<< getCurrentTime
  b <- newIORef buildBoard
  p <- newIORef (0, 0)
  lpp <- newIORef Nothing
  return State
    { board = b
    , lastUpdate = t
    , pan = p
    , lastPanPos = lpp
    }

hexPoly :: [(Float, Float)]
hexPoly = [
    (1.0000, 0.0000),
    (0.5000, 0.8660),
    (-0.5000, 0.8660),
    (-1.0000, 0.0000),
    (-0.5000, -0.8660),
    (0.5000, -0.8660),
    (1.0000, -0.0000)
  ]

toVertex (x, y) = Vertex3 x y 0.0

-- resolve overloading, not needed in "real" programs
vertex3f = vertex :: Vertex3 GLfloat -> IO ()

hexVertices = mapM_ (vertex3f . toVertex) hexPoly

gridSize :: GLdouble
gridSize = 5.0

display :: State -> DisplayCallback
display state = do
  clear [ColorBuffer, DepthBuffer]

  b <- get $ board state
  p <- get $ pan state

  let t = _clock b
  let ts = _transitions b
  let delta = _sinceLastUpdate b
  let ps = map (applyTransitions ts delta) (_grid b)

  preservingMatrix $ do
    translate $ Vector3 (fst p) (snd p) 0

    forM_ ps $ \(Placement pos (Radians o'), d) ->
      let o = o' * (180 / pi) in

      case d of
        GrabberPiece grabber -> do
          preservingMatrix $ do
            let (x, y) = hexToPixel pos
            color (Color3 1 1 (1 :: GLfloat))
            translate $ Vector3 x y (0 :: Double)
            rotate 90 $ Vector3 (0 :: Float) 0 1
            scale 0.95 0.95 (1.0 :: Float)
            renderPrimitive Polygon hexVertices

          preservingMatrix $ do
            let (x, y) = hexToPixel pos
            translate $ Vector3 x y (0.1 :: Double)
            rotate o $ Vector3 (0 :: Double) 0 1 -- TODO: toDegrees?
            translate $ Vector3 (sqrt 3 :: Float) 0 0
            rotate 90 $ Vector3 (0 :: Float) 0 1
            scale 0.95 0.95 (1.0 :: Float)

            when (grabber ^. closed) (color (Color3 0.0 0.7 (0.0 :: GLfloat)))

            renderPrimitive LineLoop hexVertices

          case grabber ^. contents of
            Nothing -> return ()
            Just (Lattice xs) -> preservingMatrix $ do
              let (x, y) = hexToPixel pos

              translate $ Vector3 x y (0 :: Double)
              rotate o $ Vector3 (0 :: Double) 0 1
              translate $ Vector3 (sqrt 3 :: Float) 0 0

              forM_ xs $ \(lpos, element) ->
                preservingMatrix $ do
                  let (x, y) = hexToPixel lpos
                  translate $ Vector3 x y (0 :: Double)
                  rotate 90 $ Vector3 (0 :: Float) 0 1
                  scale 0.90 0.90 (1.0 :: Float)
                  color (Color3 0.7 0.0 (0.0 :: GLfloat))
                  renderPrimitive Polygon hexVertices

        ReagentPiece Reagent { rlayout = Lattice xs } ->
          preservingMatrix $ do
            let (x, y) = hexToPixel pos
            translate $ Vector3 x y (0 :: Double)
            forM_ xs $ \(lpos, element) ->
              preservingMatrix $ do
                let (x, y) = hexToPixel lpos
                translate $ Vector3 x y (0 :: Double)
                rotate 90 $ Vector3 (0 :: Float) 0 1
                scale 0.90 0.90 (1.0 :: Float)
                color (Color3 0.7 0.0 (0.0 :: GLfloat))
                renderPrimitive Polygon hexVertices
        LatticePiece (Lattice xs) ->
          preservingMatrix $ do
            let (x, y) = hexToPixel pos
            translate $ Vector3 x y (0 :: Double)
            forM_ xs $ \(lpos, element) ->
              preservingMatrix $ do
                let (x, y) = hexToPixel lpos
                translate $ Vector3 x y (0 :: Double)
                rotate 90 $ Vector3 (0 :: Float) 0 1
                scale 0.90 0.90 (1.0 :: Float)
                color (Color3 0.7 0.0 (0.0 :: GLfloat))
                renderPrimitive Polygon hexVertices
        ProductPiece Product { playout = Lattice xs } ->
          preservingMatrix $ do
            let (x, y) = hexToPixel pos
            translate $ Vector3 x y (0 :: Double)
            forM_ xs $ \(lpos, element) ->
              preservingMatrix $ do
                let (x, y) = hexToPixel lpos
                translate $ Vector3 x y (0 :: Double)
                rotate 90 $ Vector3 (0 :: Float) 0 1
                scale 0.90 0.90 (1.0 :: Float)
                color (Color3 0.7 0.0 (0.0 :: GLfloat))
                renderPrimitive LineLoop hexVertices

  color (Color3 1.0 1.0 (1.0 :: GLfloat))
  currentRasterPosition $= Vertex4 (-4.7) (-4.7) 0 1
  renderString Helvetica10 (show t)

  -- don't wait!
  -- start processing buffered OpenGL routines
  swapBuffers

myInit :: IO ()
myInit = do
  -- select clearing color
  clearColor $= Color4 0 0 0 0

  depthFunc $= Just Lequal
  -- initialize viewing values
  matrixMode $= Projection
  loadIdentity
  ortho (-gridSize) gridSize (-gridSize) gridSize (-1) 1

gameLoop :: State -> IO ()
gameLoop state = do
  t <- get (lastUpdate state)
  t' <- getCurrentTime -- TODO: Handle time jumps
  b <- get (board state)

  let d = realToFrac (diffUTCTime t' t) / stepTime
  let b' = if d + (b ^. sinceLastUpdate) >= 1 then
             -- TODO: Some protection for falling behind maybe
             trace ("\n\n=================\n" ++ (show $ stepBoard b))
             (stepBoard b) & sinceLastUpdate -~ 1
           else
             b & sinceLastUpdate +~ d

  writeIORef (lastUpdate state) t'
  writeIORef (board state) b'

  postRedisplay Nothing

stepTime = 0.5
stepMs = round $ stepTime * 1000

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitSuccess
keyboard _ _ _ _ = return ()

diffPos :: Position -> Position -> Position
diffPos (Position x1 y1) (Position x2 y2) = Position (x1 - x2) (y1 - y2)

addPan :: Double -> (Double, Double) -> Position -> (Double, Double)
addPan scalingFactor (x1, y1) (Position x2 y2) =
  (x1 + fromIntegral x2 / scalingFactor, y1 - fromIntegral y2 / scalingFactor)

mouseDrag :: State -> MotionCallback
mouseDrag state pos = do
  lpp <- get (lastPanPos state)
  case lpp of
    Nothing -> return ()
    Just p -> do
      let relativePos = diffPos pos p

      pn <- get (pan state)
      -- TODO: Handle non-square windows
      Size sx _ <- get windowSize
      let scalingFactor = fromIntegral sx / (gridSize * 2)
      let pn' = addPan scalingFactor pn relativePos
      writeIORef (pan state) $ pn'
      writeIORef (lastPanPos state) $ Just pos

mouse :: State -> MouseCallback
mouse state RightButton Down pos = writeIORef (lastPanPos state) (Just pos)
mouse state RightButton Up pos = writeIORef (lastPanPos state) Nothing
mouse _ _ _ _ = return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
  initialWindowSize $= Size 250 250
  initialWindowPosition $= Position 100 100
  _ <- createWindow "Mopus Gagnum"
  state <- mkState
  myInit
  displayCallback $= display state
  keyboardMouseCallback $= Just keyboard
  motionCallback $= Just (mouseDrag state)
  mouseCallback $= Just (mouse state)
  idleCallback $= (Just $ gameLoop state)
  mainLoop
