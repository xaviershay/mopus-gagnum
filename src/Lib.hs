module Lib
    ( someFunc
    ) where

type Position = (Integer, Integer)

data Trigger = TimeTrigger Integer deriving (Show)

data Action = RotateAntiClockwise | RotateClockwise | ClawClose | ClawOpen deriving (Show)

type Program = [(Trigger, [Action])]

data Grabber = Grabber {
  program :: Program
} deriving (Show)

data Element = Fire | Water | Earth | Air deriving (Show)

data Lattice a = Lattice [(Position, a)] deriving (Show) -- TODO: Add bonds

-- TODO: combine Reagent / Product definitions somehow?
data Reagent = Reagent {
  rlayout :: Lattice Element
} deriving (Show)

data Product = Product {
  playout :: Lattice Element
} deriving (Show)

data Piece = GrabberPiece Grabber | ReagentPiece Reagent | ProductPiece Product deriving (Show)

data Board = Board {
    clock :: Integer
  , pieces :: [(Position, Position, Piece)]
  --tracks :: [Track]
} deriving (Show)

placePiece b o p = (b, o, p)

buildBoard = Board {
  --tracks = [],
  clock = 0,
  pieces = [placePiece (0, 0) (1, 0) (GrabberPiece $ Grabber { program =
    [ (TimeTrigger 0, [ClawClose
    , RotateClockwise
    , ClawOpen
    , RotateAntiClockwise
    ])]}),
    placePiece (1, 0) (0, 0) (ReagentPiece $ Reagent { rlayout =
      Lattice [((0, 0), Fire)]}),
    placePiece (1, -1) (0, 0) (ProductPiece $ Product { playout =
      Lattice [((0, 0), Fire)]})
  ]
}

someFunc :: IO ()
someFunc = putStrLn $ show buildBoard
