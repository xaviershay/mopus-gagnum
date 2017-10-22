module Main where

import Lib
import Graphics.UI.GLUT
import Data.IORef ( IORef, newIORef, writeIORef )
import Control.Monad (forM_)

data State = State
  { board :: IORef Board
  }

mkState :: IO State
mkState = do
  b <- newIORef buildBoard
  return $ State
    { board = b
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

  forM_ ps $ \(pos, orientation, d) -> do
    case d of
      GrabberPiece _ -> do
        preservingMatrix $ do
          let (x, y) = hexToPixel pos
          translate $ Vector3 x y 0
          rotate 90 $ Vector3 (0 :: Float) 0 1
          scale 0.95 0.95 (1.0 :: Float)
          renderPrimitive Polygon $ mapM_ vertex3f $ map toVertex hexPoly

        preservingMatrix $ do
          let (x, y) = hexToPixel orientation
          translate $ Vector3 x y 0
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
  b <- get (board state)
  putStrLn $ show b

  let b' = stepBoard b

  writeIORef (board state) b'

  addTimerCallback stepMs (gameLoop state)
  postRedisplay Nothing

stepMs = 500

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
  addTimerCallback stepMs (gameLoop state)
  mainLoop
