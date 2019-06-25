module ReduxGame.WrappedReduxImpl
  ( ReduxEvent
  , Events
  , ReduxW
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

redux' :: ReduxW a
redux' = ReduxW $ const return

reduxDo' :: ReduxW w -> w -> Events () -> IO w
reduxDo' (ReduxW r) w a = do
  ((), events) <- runWriterT a
  handleRemainingEvents r w events

reduxCons' :: ReduxW w -> ReduxW w -> ReduxW w
reduxCons' (ReduxW redux1) (ReduxW redux2) = ReduxW $ \e w -> return w >>= redux1 e >>= redux2 e

reduxBind' :: ReduxEvent a => ReduxW w -> (a -> w -> Events w) -> ReduxW w
reduxBind' (ReduxW redux) f = ReduxW $ \e w -> return w >>= redux e >>= focus f e

connect' :: Updater a b -> ReduxW a -> ReduxW b
connect' lens (ReduxW f) = ReduxW $ \e w -> (lens %%~ (f e)) w

class ARedux r where
  redux :: r a
  reduxDo :: r a -> a -> Events () -> IO a
  reduxCons :: r a -> r a -> r a
  reduxBind :: ReduxEvent e => r a -> (e -> a -> Events a) -> r a
  connect :: Updater a b -> r a -> r b

instance ARedux ReduxW where
  redux = redux'
  reduxDo = reduxDo'
  reduxCons = reduxCons'
  reduxBind = reduxBind'
  connect = connect'
