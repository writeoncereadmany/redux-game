module ReduxGame.Entities.EntityRedux where

import ReduxGame.Redux
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ComponentStore
import ReduxGame.Entities.Component
import ReduxGame.Entities.Entities

data EntityEvent = EntityEvent (Entities ()) deriving ReduxEvent

data EntityThenEvent = forall a . EntityThenEvent (Entities a) (a -> Events ()) deriving ReduxEvent

spawn :: Entity -> Events ()
spawn entity = fireEvent $ EntityEvent (do doCreateEntity entity; return ())

spawnThen :: Entity -> (EntityId -> Events()) -> Events ()
spawnThen entity followup = fireEvent $ EntityThenEvent (doCreateEntity entity) followup

destroy :: EntityId -> Events ()
destroy entId = fireEvent $ EntityEvent (doDestroyEntity entId)

update :: (Component a, Component b) => EntityId -> (a -> b) -> Events ()
update entId f = fireEvent $ EntityEvent (doUpdateEntity entId f)

handleEntityEvent :: Store s => EntityEvent -> ComponentStore s -> ComponentStore s
handleEntityEvent (EntityEvent action) store = updateState action store

handleEntityThenEvent :: Store s => EntityThenEvent -> ComponentStore s -> Events (ComponentStore s)
handleEntityThenEvent (EntityThenEvent action followup) store = do
  let (a, store') = runEntities action store
  followup a
  return store'

entityRedux :: Store s => Redux (ComponentStore s)
entityRedux = redux
          |-> handleEntityEvent
          |=> handleEntityThenEvent

infixl 1 |$>
(|$>) :: (ReduxEvent a, Component b, Component c, Store s)
      => Redux (ComponentStore s)
      -> (a -> b -> c)
      -> Redux (ComponentStore s)
redux |$> f = redux |-> apply . f

infixl 1 |*>
(|*>) :: (ReduxEvent a, Component b, Component c, Store s)
      => Redux (ComponentStore s)
      -> (a -> b -> Events c)
      -> Redux (ComponentStore s)
redux |*> f = redux |=> applyM . f
