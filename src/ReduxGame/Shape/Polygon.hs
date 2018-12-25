module ReduxGame.Shape.Polygon (windClockwise, normals) where

import Data.Maybe
import Graphics.Gloss.Data.Vector

import ReduxGame.Shape.NumVectorInstance

duplicateEnd :: [ Vector ] -> [ Vector ]
duplicateEnd [] = []
duplicateEnd xs = (last xs) : xs

edges :: [ Vector ] -> [ Vector ]
edges points = edges' $ duplicateEnd points where
  edges' (a : b : rest) = (b - a) : edges' (b : rest)
  edges' _ = []

data Winding = Clockwise | Anticlockwise deriving (Eq, Show)

windClockwise :: [ Vector ] -> [ Vector ]
windClockwise points = let windings = catMaybes $ allWindings $ duplicateEnd $ edges points
                        in reverseIfNeeded windings where
  determineWinding :: Vector -> Vector -> Maybe Winding
  determineWinding a b = case signum $ (anticlockwise_normal a) `dotV` b of
    0    -> Nothing
    1    -> Just Anticlockwise
    (-1) -> Just Clockwise

  allWindings :: [ Vector ] -> [ Maybe Winding ]
  allWindings (a:b:rest) = determineWinding a b : allWindings (b:rest)
  allWindings _ = []

  reverseIfNeeded :: [ Winding ] -> [ Vector ]
  reverseIfNeeded windings
    | all (== Clockwise) windings = points
    | all (== Anticlockwise) windings = reverse points
    | otherwise = error "Concave polygon"


anticlockwise_normal :: Vector -> Vector
anticlockwise_normal (x, y) = ((-y), x)

normals :: [ Vector ] -> [ Vector ]
normals points = normalizeV <$> anticlockwise_normal <$> edges points
