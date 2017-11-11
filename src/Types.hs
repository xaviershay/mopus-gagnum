{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

type Position = (Integer, Integer)

newtype Radians = Radians Double deriving (Show, Eq, Num, Fractional, Floating, Real, Ord)

data Element = Fire | Water | Earth | Air deriving (Show, Eq)

data Placement = Placement Position Radians deriving (Show, Eq)

instance Monoid Placement where
  mempty = Placement (0, 0) 0
  Placement (x1, y1) r1 `mappend` Placement (x2, y2) r2 =
    Placement (x1 + x2, y1 + y2) (r1 + r2)

