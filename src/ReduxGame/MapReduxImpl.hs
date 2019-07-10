module ReduxGame.MapReduxImpl where

import Control.Monad
import Control.Monad.Writer
import Control.Lens
import Data.Typeable
import Data.ConstrainedDynamic
import Data.DList
import Data.Map as M

import Debug.Trace

import ReduxGame.ARedux

data MapRedux state = MapRedux (M.Map TypeRep (DynEvent -> state -> Events state))

typeOfFirstArgument :: forall a b . Typeable a => (a -> b -> Events b) -> TypeRep
typeOfFirstArgument f = let proxy :: Proxy a = Proxy in typeRep proxy

dynacompose :: (DynEvent -> state -> Events state)
            -> (DynEvent -> state -> Events state)
            -> DynEvent -> state -> Events state
dynacompose f1 f2 event = f1 event >=> f2 event

focus :: ReduxEvent event
      => (event -> state -> Events state)
      -> DynEvent -> state -> Events state
focus f dynEvent state = case fromDynamic dynEvent of
  (Just event) -> return state >>= f event
  Nothing -> return state

dispatch :: MapRedux state -> DynEvent -> state -> Events state
dispatch (MapRedux m) event state = do
  let typerep = dynTypeRep event
  case M.lookup (dynTypeRep event) m of
    Nothing -> return state
    (Just f) -> return state >>= f event

handleRemainingEvents :: MapRedux w -> w -> DList DynEvent -> IO w
handleRemainingEvents f world events = do
  (world', events') <- runWriterT $ foldM (flip (dispatch f)) world events
  case events' of
    Nil -> return world'
    otherwise -> handleRemainingEvents f world' events'

reduxDo' :: MapRedux state -> state -> Events () -> IO state
reduxDo' redux state eventsAction = do
  ((), events) <- runWriterT eventsAction
  handleRemainingEvents redux state events

instance ARedux MapRedux where
  redux = MapRedux $ M.empty
  reduxDo = reduxDo'
  reduxCons (MapRedux a) (MapRedux b) = MapRedux (M.unionWith dynacompose a b)
  reduxFocus f = MapRedux $ M.singleton (typeOfFirstArgument f) (focus f)
  connect lens (MapRedux inner) = MapRedux (M.map (\f e w -> (lens %%~ (f e)) w) inner)
