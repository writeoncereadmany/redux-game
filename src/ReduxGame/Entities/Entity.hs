module ReduxGame.Entities.Entity where

import Data.Typeable
import Data.Maybe

data Tagged a = Tagged EntityId a

instance Functor Tagged where
  fmap f (Tagged entId a) = Tagged entId (f a)

class Components c where
  allComponents :: Component a => c -> [ Tagged a ]
  componentById :: Component a => EntityId -> c -> Maybe a
  updateComponents :: Component a => [ Tagged (Maybe a) ] -> c -> c

class Typeable a => Component a where
  getAll :: Components c => c -> [ Tagged a ]
  getAll = allComponents
  getById :: Components c => EntityId -> c -> Maybe a
  getById = componentById
  setAll :: Components c => [ Tagged (Maybe a) ] -> c -> c
  setAll = updateComponents

instance (Component a, Component b) => Component (a, b) where
  getAll c = catMaybes $ addB <$> getAll c where
    addB :: Tagged a -> Maybe (Tagged (a, b))
    addB (Tagged e a) = do
      b <- getById e c
      return $ Tagged e (a, b)
  getById e c = do
    a <- getById e c
    b <- getById e c
    return (a, b)
  setAll xs c = setAll (((fst <$>) <$>) <$> xs) . setAll (((snd <$>) <$>) <$> xs) $ c

instance (Component a) => Component (Maybe a) where
  getAll _ = []  -- ignoring for now
  getById e c = Just $ getById e c
  setAll xs c = c -- also ignoring for now

class Default a where
  defaultValue :: a
  orDefault :: Maybe a -> a
  orDefault = fromMaybe defaultValue

type EntityId = Integer

data Property = forall c . Component c => Property c

data Entity = Entity [ Property ]

entity :: Entity
entity = Entity []

infixl 1 <-+

(<-+) :: Component c => Entity -> c -> Entity
(<-+) (Entity props) comp = Entity $ Property comp : props

infixl 1 <++

(<++) :: Entity -> Entity -> Entity
(<++) (Entity props1) (Entity props2) = Entity $ props1 ++ props2
