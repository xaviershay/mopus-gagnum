module Main where

import Lib
import Graphics.UI.GLUT
import Data.IORef ( IORef, newIORef, writeIORef )
import Data.Time.Clock ( getCurrentTime, diffUTCTime, UTCTime )
import Control.Monad (forM_)
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )

data State = State
  { board :: IORef Board
  , lastUpdate :: IORef UTCTime
  }

mkState :: IO State
mkState = do
  t <- newIORef =<< getCurrentTime
  b <- newIORef buildBoard
  return $ State
    { board = b
    , lastUpdate = t
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

gridSize :: GLdouble
gridSize = 5.0

toVertex (x, y) = Vertex3 x y 0.0

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

display :: State -> DisplayCallback
display state = do
  -- clear all pixels
  clear [ ColorBuffer ]

  -- draw white polygon (rectangle) with corners at
  -- (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0)
  color (Color3 1.0 1.0 (1.0 :: GLfloat))
  -- resolve overloading, not needed in "real" programs
  let vertex3f = vertex :: Vertex3 GLfloat -> IO ()

  b <- get $ board state
  let t = clock b
  let ps = pieces b

  forM_ ps $ \(pos, (o, o'), d) -> do
    case d of
      GrabberPiece _ -> do
        preservingMatrix $ do
          let (x, y) = hexToPixel pos
          translate $ Vector3 x y 0
          rotate 90 $ Vector3 (0 :: Float) 0 1
          scale 0.95 0.95 (1.0 :: Float)
          renderPrimitive Polygon $ mapM_ vertex3f $ map toVertex hexPoly

        preservingMatrix $ do
          let (x, y) = hexToPixel pos
          translate $ Vector3 x y 0
          rotate (fromIntegral o') $ Vector3 (0 :: Float) 0 1
          translate $ Vector3 (sqrt(3) :: Float) 0 0
          rotate 90 $ Vector3 (0 :: Float) 0 1
          scale 0.95 0.95 (1.0 :: Float)
          renderPrimitive LineLoop $ mapM_ vertex3f $ map toVertex hexPoly
      ReagentPiece Reagent { rlayout = Lattice xs } -> do
        preservingMatrix $ do
          let (x, y) = hexToPixel pos
          translate $ Vector3 x y 0
          forM_ xs $ \(lpos, element) -> do
            preservingMatrix $ do
              let (x, y) = hexToPixel lpos
              translate $ Vector3 x y 0
              rotate 90 $ Vector3 (0 :: Float) 0 1
              scale 0.90 0.90 (1.0 :: Float)
              color (Color3 0.7 0.0 (0.0 :: GLfloat))
              renderPrimitive Polygon $ mapM_ vertex3f $ map toVertex hexPoly
      ProductPiece Product { playout = Lattice xs } -> do
        preservingMatrix $ do
          let (x, y) = hexToPixel pos
          translate $ Vector3 x y 0
          forM_ xs $ \(lpos, element) -> do
            preservingMatrix $ do
              let (x, y) = hexToPixel lpos
              translate $ Vector3 x y 0
              rotate 90 $ Vector3 (0 :: Float) 0 1
              scale 0.90 0.90 (1.0 :: Float)
              color (Color3 0.7 0.0 (0.0 :: GLfloat))
              renderPrimitive LineLoop $ mapM_ vertex3f $ map toVertex hexPoly

  color (Color3 1.0 1.0 (1.0 :: GLfloat))
  currentRasterPosition $= Vertex4 (-4.7) (-4.7) 0 1
  renderString Helvetica10 (show t)

  -- don't wait!
  -- start processing buffered OpenGL routines
  flush

myInit :: IO ()
myInit = do
  -- select clearing color
  clearColor $= Color4 0 0 0 0

  -- initialize viewing values
  matrixMode $= Projection
  loadIdentity
  ortho (-gridSize) gridSize (-gridSize) gridSize (-1) 1

gameLoop :: State -> IO ()
gameLoop state = do
  t <- get (lastUpdate state)
  t' <- getCurrentTime -- TODO: Handle time jumps
  b <- get (board state)

  let d = realToFrac $ diffUTCTime t' t
  let b' = if d + (sinceLastUpdate b) >= stepTime then
             -- TODO: Some protection for falling behind maybe
             (stepBoard b) { sinceLastUpdate = (sinceLastUpdate b) - stepTime }
           else
             b { sinceLastUpdate = (sinceLastUpdate b) + d }

  writeIORef (lastUpdate state) t'
  writeIORef (board state) b'

  postRedisplay Nothing

stepTime = 0.5
stepMs = round $ stepTime * 1000

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _ _ _ _ = return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [ SingleBuffered, RGBMode ]
  initialWindowSize $= Size 250 250
  initialWindowPosition $= Position 100 100
  _ <- createWindow "Mopus Gagnum"
  state <- mkState
  myInit
  displayCallback $= display state
  keyboardMouseCallback $= Just keyboard
  idleCallback $= (Just $ gameLoop state)
  mainLoop
