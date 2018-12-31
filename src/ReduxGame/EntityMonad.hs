module ReduxGame.EntityMonad where

import Data.Maybe
import Data.Typeable
import Data.ConstrainedDynamic

type EntityId = Integer

class Typeable a => Component a

data Tagged a = Tagged EntityId a

data DynamicStore where
  DynamicStore :: forall a . Component a => [ Tagged a ] -> DynamicStore

toStore :: Component a => [ Tagged a ] -> DynamicStore
toStore = DynamicStore

fromStore :: Component a => DynamicStore -> Maybe [ Tagged a ]
fromStore (DynamicStore b) = cast b

data EntityMonad = EntityMonad [ DynamicStore ]

storeOf :: Component a => EntityMonad -> [ Tagged a ]
storeOf (EntityMonad stores) = storeOf' stores where
  storeOf' [] = []
  storeOf' (x:xs) = case fromStore x of
    Just a  -> a
    Nothing -> storeOf' xs

typesMatch :: a -> Maybe a -> Bool
typesMatch _ x = isJust x

replaceStore :: Component a => [ Tagged a] -> EntityMonad -> EntityMonad
replaceStore newStore (EntityMonad oldStores) =
  EntityMonad $ replaceStore' newStore oldStores where
    replaceStore' :: Component a => [Tagged a] -> [DynamicStore] -> [DynamicStore]
    replaceStore' newStore [] = [ toStore newStore ]
    replaceStore' newStore (oldStore : oldStores) =
      if (typesMatch newStore (fromStore oldStore))
        then (toStore newStore) : oldStores
        else oldStore : replaceStore' newStore oldStores
