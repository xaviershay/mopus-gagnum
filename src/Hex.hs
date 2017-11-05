-- Hex functions translated from Red Blob Games' articles
module Hex where

hexToPixel :: RealFloat a => (Integer, Integer) -> (a, a)
hexToPixel (q, r) =
  (x, y)
  where
    x = sqrt 3 * (fromInteger q + fromInteger r / 2.0)
    y = -3 / 2 * fromInteger r

pixelToHex :: RealFloat a => (a, a) -> (Integer, Integer)
pixelToHex (x, y) =
  hexRound (q, r)
  where
    q = x * sqrt 3 / 3 - y / (-3)
    r = y * (-2)/3

cubeRound :: RealFloat a => (a, a, a) -> (Integer, Integer, Integer)
cubeRound (x, y, z)
  | x_diff > y_diff && x_diff > z_diff = ((-ry) - rz, ry, rz)
  | y_diff > z_diff                    = (rx, (-rx) - rz, rz)
  | otherwise                          = (rx, ry, (-rx) - ry)
  where
    rx = round x :: Integer
    ry = round y :: Integer
    rz = round z :: Integer
    x_diff = abs (fromIntegral rx - x)
    y_diff = abs (fromIntegral ry - y)
    z_diff = abs (fromIntegral rz - z)

axialToCube (q, r) = (q, -q-r, r)
cubeToAxial (x, y, z) = (x, z)

hexRound :: RealFloat a => (a, a) -> (Integer, Integer)
hexRound = cubeToAxial . cubeRound . axialToCube
