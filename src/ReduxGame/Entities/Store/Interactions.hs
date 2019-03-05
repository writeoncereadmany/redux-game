module ReduxGame.Entities.Store.Interactions where

import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ComponentStore
import ReduxGame.Entities.Store.Variadics

class (Extractable a, Persistable a) => Replaceable a

relate :: (Extractable a, Extractable b, Monad m, Store s)
       => (a -> b -> m ())
       -> ComponentStore s
       -> m (ComponentStore s)
relate f cs = do
  let as = content <$> extract cs
  let bs = content <$> extract cs
  sequence [f a b | a <- as, b <- bs]
  return cs

selfRelate :: (Extractable a, Monad m, Store s)
           => (a -> a -> m ())
           -> ComponentStore s
           -> m (ComponentStore s)
selfRelate f cs = do
  let as = content <$> extract cs
  disjointPairs as
  return cs where
    disjointPairs [] = return []
    disjointPairs (x:xs) = do
      mapM (f x) xs
      disjointPairs xs

selfRelate2 :: (Extractable a, Monad m, Store s)
            => (a -> a -> m ())
            -> ([a] -> [(a, a)])
            -> ComponentStore s
            -> m (ComponentStore s)
selfRelate2 f broadphase cs = do
  traverse (uncurry f) (broadphase $ content <$> extract cs)
  return cs

naiveBroadphase :: [Tagged a] -> [(Tagged a, Tagged a)]
naiveBroadphase as = [(a, b) | a <- as, b <- as, a `lowerEntityId` b ] where
  lowerEntityId (Tagged id_a _) (Tagged id_b _) = id_a < id_b
