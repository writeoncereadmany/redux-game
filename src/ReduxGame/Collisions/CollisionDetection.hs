module ReduxGame.Collisions.CollisionDetection
  ( (!!!)
  , (!!>)
  ) where

import Graphics.Gloss.Data.Vector
import Data.Maybe
import ReduxGame.Shape (Shape (Polygon, Circle))
import Control.Applicative

-- to determine which vector is longer without bothering wih the expensive sqrt
sqMagV :: Vector -> Float
sqMagV (x, y) = x * x + y * y

shorter :: Vector -> Vector -> Vector
shorter x y = if sqMagV x < sqMagV y then x else y

project :: Shape -> Vector -> (Float, Float)
project (Polygon points _) normal = let projections = (dotV normal) <$> points
                              in (foldl1 min projections, foldl1 max projections)
project (Circle c r) normal = let p = dotV normal c
                               in (p - r, p + r)

pushProj :: (Float, Float) -> (Float, Float) -> Maybe Float
pushProj (min1, max1) (min2, max2)
  | min1 >= max2 = Nothing
  | max1 <= min2 = Nothing
  | otherwise   = Just $ smaller (min1 - max2) (max1 - min2)
    where smaller a b = if abs a < abs b then a else b

push :: Shape -> Shape -> Vector -> Maybe Vector
push a b normal = let proj1 = project a normal
                      proj2 = project b normal
                      pushDist = pushProj proj1 proj2
                   in (flip mulSV normal) <$> pushDist

unitBetween :: Vector -> Vector -> Vector
unitBetween a b = normalizeV (b - a)

normals :: Shape -> Shape -> [ Vector ]
normals (Circle c1 _) (Circle c2 _) = [ unitBetween c1 c2 ]
normals (Polygon _ n1s) (Polygon _ n2s) = n1s ++ n2s
normals (Polygon ps ns) (Circle c _) = normalizeV (foldl1 shorter $ (c -) <$> ps) : ns
normals (Circle c _) (Polygon ps ns) = normalizeV (foldl1 shorter $ (c -) <$> ps) : ns

infixl 2 !!!
infixl 2 !!>

(!!>) :: Shape -> Shape -> Maybe Vector
(!!>) a b = foldl1 (liftA2 shorter) $ push a b <$> normals a b

(!!!) :: Shape -> Shape -> Bool
(!!!) a b = isJust $ a !!> b
