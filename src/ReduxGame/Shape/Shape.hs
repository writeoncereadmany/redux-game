module ReduxGame.Shape.Shape where

import Graphics.Gloss (Vector)

import ReduxGame.Shape.NumVectorInstance
import ReduxGame.Shape.Polygon

data Shape = Circle Vector Float
           | Polygon [ Vector ] [ Vector ]
           deriving (Show, Eq)

move :: Vector -> Shape -> Shape
move v (Circle c r) = Circle (v + c) r
move v (Polygon ps ns) = Polygon ((v+) <$> ps) ns

circle :: Vector -> Float -> Shape
circle = Circle

polygon :: [ Vector ] -> Shape
polygon points = let clockwise = windClockwise points
                  in Polygon clockwise (normals clockwise)

rectangle :: Vector -> Vector -> Shape
rectangle (x, y) (w, h) = polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]
