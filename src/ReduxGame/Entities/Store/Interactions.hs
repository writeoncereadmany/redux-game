module ReduxGame.Entities.Store.Interactions where

import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ComponentStore
import ReduxGame.Entities.Store.Variadics

class (Extractable a, Persistable a) => Replaceable a

interact :: (Extractable a, Extractable b, Monad m, Store s)
         => (a -> b -> m ())
         -> ComponentStore s
         -> m (ComponentStore s)
interact f cs = do
  let as = content <$> extract cs
  let bs = content <$> extract cs
  sequence [f a b | a <- as, b <- bs]
  return cs
