module ReduxGame.Entities.ComponentStore where

import Data.Maybe
import ReduxGame.Entities.Components

data ComponentStore = ComponentStore [ Components ]

emptyStore :: ComponentStore
emptyStore = ComponentStore []

storeOf :: Component a => ComponentStore -> Store a
storeOf (ComponentStore stores) = storeOf' stores where
  storeOf' [] = []
  storeOf' (x:xs) = case fromStore x of
    Just a  -> a
    Nothing -> storeOf' xs

typesMatch :: a -> Maybe a -> Bool
typesMatch _ x = isJust x

replaceStore :: Component a => Store a -> ComponentStore -> ComponentStore
replaceStore newStore (ComponentStore oldStores) =
  ComponentStore $ replaceStore' newStore oldStores where
    replaceStore' :: Component a => Store a -> [ Components ] -> [ Components ]
    replaceStore' newStore [] = [ Components newStore ]
    replaceStore' newStore (oldStore : oldStores) =
      if (typesMatch newStore (fromStore oldStore))
        then (Components newStore) : oldStores
        else oldStore : replaceStore' newStore oldStores
