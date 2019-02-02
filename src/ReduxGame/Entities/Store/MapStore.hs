module ReduxGame.Entities.Store.MapStore where

import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Entity
import qualified Data.Map as M

data MapStore a = MapStore (M.Map EntityId a)

instance Store MapStore where
  withId entId (MapStore m) = M.lookup entId m
  components (MapStore m) = (\(t, a) -> Tagged t a) <$> M.toList m
  mergeComponents news (MapStore m) = let
       news' = M.fromList $ (\(Tagged entId a) -> (entId, a)) <$> news
    in MapStore $ M.union news' m
  emptyStore = MapStore M.empty
  delete entId (MapStore m) = MapStore $ M.delete entId m
