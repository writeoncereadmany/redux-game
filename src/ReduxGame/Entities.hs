module ReduxGame.Entities
  ( module ReduxGame.Entities.Entity
  -- , module ReduxGame.Entities.Entities
  -- entity redux
  , (|$>)
  , (|*>)
  , spawn
  , spawnThen
  , destroy
  -- variadics support
  , Only (Only)
  , Extractable (extract)
  , foldStore 
  , extract_2r1d
  -- definition of world type
  , World
  , newWorld
  , worldRedux
) where

-- for exporting
import ReduxGame.Entities.Entity
import ReduxGame.Entities.Entities
import ReduxGame.Entities.EntityRedux
import ReduxGame.Entities.Store.Interactions
import ReduxGame.Entities.Store.Variadics

-- to support new definitions, linking all these bits together
import ReduxGame.Entities.Store.ComponentStore
import ReduxGame.Entities.Store.MapStore
import ReduxGame.Redux

type World = ComponentStore MapStore
newWorld = emptyComponents

worldRedux :: Redux World
worldRedux = entityRedux
