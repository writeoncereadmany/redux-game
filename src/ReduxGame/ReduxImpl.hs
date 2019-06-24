module ReduxGame.ReduxImpl
  ( ReduxEvent
  , Events
  , ReduxFun
  , redux
  , reduxDo
  , reduxCons
  , reduxBind
  , connect
  ) where

import Control.Lens
import Control.Monad.Writer
import Data.Typeable
import Data.ConstrainedDynamic
import Data.DList

import Graphics.Gloss.Interface.IO.Game

class (Typeable a) => ReduxEvent a

type DynEvent = ConstrainedDynamic ReduxEvent
type Events w = WriterT (DList DynEvent) IO w
type Updater a b = forall l . (Functor l, Applicative l) => LensLike' l b a
type ReduxFun w = DynEvent -> w -> Events w

instance ReduxEvent Event

focus :: (ReduxEvent a) => (a -> b -> Events b) -> DynEvent -> b -> Events b
focus f e w = case (fromDynamic e) of
  Just x -> f x w
  Nothing -> return w

handleRemainingEvents :: ReduxFun w -> w -> DList DynEvent -> IO w
handleRemainingEvents f world events = do
  (world', events') <- runWriterT $ foldM (flip f) world events
  case events' of
    Nil -> return world'
    otherwise -> handleRemainingEvents f world' events'

redux :: ReduxFun a
redux = const return

reduxDo :: ReduxFun w -> w -> Events () -> IO w
reduxDo r w a = do
  ((), events) <- runWriterT a
  handleRemainingEvents r w events

reduxCons :: ReduxFun w -> ReduxFun w -> ReduxFun w
reduxCons redux1 redux2 e w = return w >>= redux1 e >>= redux2 e

reduxBind :: ReduxEvent a => ReduxFun w -> (a -> w -> Events w) -> ReduxFun w
reduxBind redux f e w = return w >>= redux e >>= focus f e

connect :: Updater a b -> (i -> a -> Events a) -> (i -> b -> Events b)
connect lens f e = lens %%~ (f e)
