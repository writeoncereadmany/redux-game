module ReduxGame.Entities.EntityRedux where

import ReduxGame.Redux
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ComponentStore
import ReduxGame.Entities.Store.Variadics
import ReduxGame.Entities.Entities

data EntityEvent = EntityEvent (Entities ()) deriving ReduxEvent

fireEntityChange :: Entities () -> Events ()
fireEntityChange = fireEvent . EntityEvent

handleEntityEvent :: Store s => EntityEvent -> ComponentStore s -> ComponentStore s
handleEntityEvent (EntityEvent action) store = updateState action store

entityRedux :: Store s => Redux (ComponentStore s)
entityRedux = redux
          |-> handleEntityEvent

infixl 1 |$>
(|$>) :: (ReduxEvent a, Extractable b, Updatable c, Store s)
      => Redux (ComponentStore s)
      -> (a -> b -> c)
      -> Redux (ComponentStore s)
redux |$> f = redux |-> apply . f

infixl 1 |*>
(|*>) :: (ReduxEvent a, Extractable b, Updatable c, Store s)
      => Redux (ComponentStore s)
      -> (a -> b -> Events c)
      -> Redux (ComponentStore s)
redux |*> f = redux |=> applyM . f
