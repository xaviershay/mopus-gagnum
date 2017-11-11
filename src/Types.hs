{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Data.Monoid ((<>))

data Position = Position Integer Integer deriving (Show, Eq, Ord)
instance Monoid Position where
  mempty = Position 0 0
  (Position x1 y1) `mappend` (Position x2 y2) = Position (x1 + x2) (y1 + y2)

mkPos = Position

toTuple :: Position -> (Integer, Integer)
toTuple (Position x y) = (x, y)

newtype Radians = Radians Double deriving (Show, Eq, Num, Fractional, Floating, Real, Ord)

data Element = Fire | Water | Earth | Air deriving (Show, Eq)

data Placement = Placement Position Radians deriving (Show, Eq)

instance Monoid Placement where
  mempty = Placement mempty 0
  Placement p1 r1 `mappend` Placement p2 r2 =
    Placement (p1 <> p2) (r1 + r2)
