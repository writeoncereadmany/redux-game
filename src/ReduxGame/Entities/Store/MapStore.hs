module ReduxGame.Entities.Store.MapStore where

import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Component
import qualified Data.Map as M

newtype MapStore a = MapStore (M.Map EntityId a)

instance Store MapStore where
  withId entId (MapStore m) = M.lookup entId m
  components (MapStore m) = fromPair <$> M.toList m
  mergeComponents news (MapStore m) = MapStore $ M.union (M.fromList $ toPair <$> news) m
  updateComponents maybes (MapStore m) = MapStore $ foldr update m maybes where
    update (Tagged entId maybeA) = M.alter (const maybeA) entId 
  emptyStore = MapStore M.empty
  delete entId (MapStore m) = MapStore $ M.delete entId m
