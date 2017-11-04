module Main where

import Lib
import Hex
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
  , pan :: IORef (Float, Float)
  }

mkState :: IO State
mkState = do
  t <- newIORef =<< getCurrentTime
  b <- newIORef buildBoard
  p <- newIORef (-1, 1)
  return State
    { board = b
    , lastUpdate = t
    , pan = p
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
  let ps = _grid b
  let delta = _sinceLastUpdate b

  preservingMatrix $ do
    translate $ Vector3 (fst p) (snd p) 0

    forM_ ps $ \(((pos, pos'), (o, o')), d) ->
      case d of
        GrabberPiece grabber -> do
          preservingMatrix $ do
            let (x, y) = hexToPixel pos
            color (Color3 1 1 (1 :: GLfloat))
            translate $ Vector3 x y 0
            rotate 90 $ Vector3 (0 :: Float) 0 1
            scale 0.95 0.95 (1.0 :: Float)
            renderPrimitive Polygon hexVertices

          preservingMatrix $ do
            let (x, y) = hexToPixel pos
            let o'' = fromIntegral o + (fromIntegral (o' - o) * delta)
            translate $ Vector3 x y 0.1
            rotate o'' $ Vector3 (0 :: Double) 0 1
            translate $ Vector3 (sqrt 3 :: Float) 0 0
            rotate 90 $ Vector3 (0 :: Float) 0 1
            scale 0.95 0.95 (1.0 :: Float)

            when (grabber ^. closed) (color (Color3 0.0 0.7 (0.0 :: GLfloat)))

            renderPrimitive LineLoop hexVertices

          case grabber ^. contents of
            Nothing -> return ()
            Just (Lattice xs) -> preservingMatrix $ do
              let (x, y) = hexToPixel pos
              let o'' = fromIntegral o + (fromIntegral (o' - o) * delta)

              translate $ Vector3 x y 0
              rotate o'' $ Vector3 (0 :: Double) 0 1
              translate $ Vector3 (sqrt 3 :: Float) 0 0

              forM_ xs $ \(lpos, element) ->
                preservingMatrix $ do
                  let (x, y) = hexToPixel lpos
                  translate $ Vector3 x y 0
                  rotate 90 $ Vector3 (0 :: Float) 0 1
                  scale 0.90 0.90 (1.0 :: Float)
                  color (Color3 0.7 0.0 (0.0 :: GLfloat))
                  renderPrimitive Polygon hexVertices

        ReagentPiece Reagent { rlayout = Lattice xs } ->
          preservingMatrix $ do
            let (x, y) = hexToPixel pos
            translate $ Vector3 x y 0
            forM_ xs $ \(lpos, element) ->
              preservingMatrix $ do
                let (x, y) = hexToPixel lpos
                translate $ Vector3 x y 0
                rotate 90 $ Vector3 (0 :: Float) 0 1
                scale 0.90 0.90 (1.0 :: Float)
                color (Color3 0.7 0.0 (0.0 :: GLfloat))
                renderPrimitive Polygon hexVertices
        LatticePiece (Lattice xs) ->
          preservingMatrix $ do
            let (x, y) = hexToPixel pos
            translate $ Vector3 x y 0
            forM_ xs $ \(lpos, element) ->
              preservingMatrix $ do
                let (x, y) = hexToPixel lpos
                translate $ Vector3 x y 0
                rotate 90 $ Vector3 (0 :: Float) 0 1
                scale 0.90 0.90 (1.0 :: Float)
                color (Color3 0.7 0.0 (0.0 :: GLfloat))
                renderPrimitive Polygon hexVertices
        ProductPiece Product { playout = Lattice xs } ->
          preservingMatrix $ do
            let (x, y) = hexToPixel pos
            translate $ Vector3 x y 0
            forM_ xs $ \(lpos, element) ->
              preservingMatrix $ do
                let (x, y) = hexToPixel lpos
                translate $ Vector3 x y 0
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
             trace (show $ stepBoard b)
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
  idleCallback $= (Just $ gameLoop state)
  mainLoop
