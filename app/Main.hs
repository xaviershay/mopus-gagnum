module Main where

import Lib
import Graphics.UI.GLUT
import Data.IORef ( IORef, newIORef, writeIORef )

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

display :: State -> DisplayCallback
display state = do
  -- clear all pixels
  clear [ ColorBuffer ]

  -- draw white polygon (rectangle) with corners at
  -- (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0)
  color (Color3 1.0 1.0 (1.0 :: GLfloat))
  -- resolve overloading, not needed in "real" programs
  let vertex3f = vertex :: Vertex3 GLfloat -> IO ()

  t <- get $ clock state

  preservingMatrix $ do
    renderPrimitive Polygon $ mapM_ vertex3f $ map toVertex hexPoly
    currentRasterPosition $= Vertex4 (-9.7) (-9.7) 0 1
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
  ortho (-10) 10 (-10) 10 (-1) 1

data State = State { clock :: IORef Integer }

gameLoop :: State -> IO ()
gameLoop state = do
  t <- get (clock state)

  let t' = t + 1

  writeIORef (clock state) t'

  addTimerCallback 1000 (gameLoop state)
  postRedisplay Nothing

mkState :: IO State
mkState = do
  t <- newIORef 0
  return $ State { clock = t }

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [ SingleBuffered, RGBMode ]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 100 100
  _ <- createWindow "hello"
  state <- mkState
  myInit
  displayCallback $= display state
  addTimerCallback 1000 (gameLoop state)
  mainLoop
