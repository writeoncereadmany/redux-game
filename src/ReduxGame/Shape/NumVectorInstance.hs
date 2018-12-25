module ReduxGame.Shape.NumVectorInstance where

instance (Num a) => Num (a, a) where
  (a, b) + (x, y) = (a + x, b + y)
  (a, b) - (x, y) = (a - x, b - y)
  (a, b) * (x, y) = (a * x, b * y)
  fromInteger a = (fromInteger a, fromInteger 0)
  abs (a, b) = (abs a, abs b)
  signum (a, b) = (signum a, signum b)
