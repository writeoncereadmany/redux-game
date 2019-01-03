module ReduxGame.Entities.ComponentStore where

import Data.Maybe
import Data.Typeable
import ReduxGame.Entities.Store

class Typeable a => Component a

data DynStore where
  DynStore :: forall a . Component a => Store a -> DynStore

fromStore :: Component a => DynStore -> Maybe (Store a)
fromStore (DynStore b) = cast b

data ComponentStore = ComponentStore [ DynStore ]

emptyStore :: ComponentStore
emptyStore = ComponentStore []

storeOf :: Component a => ComponentStore -> Store a
storeOf (ComponentStore stores) = storeOf' stores where
  storeOf' [] = Store []
  storeOf' (x:xs) = case fromStore x of
    Just a  -> a
    Nothing -> storeOf' xs

typesMatch :: a -> Maybe a -> Bool
typesMatch _ x = isJust x

replaceStore :: Component a => Store a -> ComponentStore -> ComponentStore
replaceStore newStore (ComponentStore oldStores) =
  ComponentStore $ replaceStore' newStore oldStores where
    replaceStore' :: Component a => Store a -> [ DynStore ] -> [ DynStore ]
    replaceStore' newStore [] = [ DynStore newStore ]
    replaceStore' newStore (oldStore : oldStores) =
      if (typesMatch newStore (fromStore oldStore))
        then (DynStore newStore) : oldStores
        else oldStore : replaceStore' newStore oldStores
