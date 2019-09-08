module ReduxGame.Entities.Interactions where

import ReduxGame.Entities.Component

relate :: (Component a, Component b, Monad m, Components c)
       => (a -> b -> m ())
       -> c
       -> m c
relate f cs = do
  let as = content <$> getAll cs
  let bs = content <$> getAll cs
  sequence [f a b | a <- as, b <- bs]
  return cs

selfRelate :: (Component a, Monad m, Components c)
           => (a -> a -> m ())
           -> ([a] -> [(a, a)])
           -> c
           -> m c
selfRelate f broadphase cs = do
  traverse (uncurry f) (broadphase $ content <$> getAll cs)
  return cs

naiveBroadphase :: [Tagged a] -> [(Tagged a, Tagged a)]
naiveBroadphase as = [(a, b) | a <- as, b <- as, a `lowerEntityId` b ] where
  lowerEntityId (Tagged id_a _) (Tagged id_b _) = id_a < id_b
