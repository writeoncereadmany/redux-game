module ReduxGame.WrappedReduxImpl
  ( ReduxEvent
  , Events
  , ReduxW
  , redux
  , reduxDo
  , reduxCons
  , reduxFocus
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
data ReduxW a = ReduxW (ReduxFun a)

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

reduxDo' :: ReduxW w -> w -> Events () -> IO w
reduxDo' (ReduxW r) w a = do
  ((), events) <- runWriterT a
  handleRemainingEvents r w events

connect' :: Updater a b -> ReduxW a -> ReduxW b
connect' lens (ReduxW f) = ReduxW $ \e w -> (lens %%~ (f e)) w

class ARedux r where
  redux :: r a
  reduxDo :: r a -> a -> Events () -> IO a
  reduxCons :: r a -> r a -> r a
  reduxFocus :: ReduxEvent e => (e -> a -> Events a) -> r a
  connect :: Updater a b -> r a -> r b

instance ARedux ReduxW where
  redux = ReduxW (const return)
  reduxDo = reduxDo'
  reduxCons (ReduxW a) (ReduxW b) = ReduxW $ \e w -> return w >>= a e >>= b e
  reduxFocus = ReduxW . focus
  connect = connect'
