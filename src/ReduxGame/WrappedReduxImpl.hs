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
import ReduxGame.ARedux

type ReduxFun w = DynEvent -> w -> Events w
data ReduxW a = ReduxW (ReduxFun a)

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

reduxDo' :: ReduxW w -> Events () -> w -> IO w
reduxDo' (ReduxW r) a w = do
  ((), events) <- runWriterT a
  handleRemainingEvents r w events

instance ARedux ReduxW where
  redux = ReduxW (const return)
  reduxDo = reduxDo'
  reduxCons (ReduxW a) (ReduxW b) = ReduxW $ \e w -> return w >>= a e >>= b e
  reduxFocus = ReduxW . focus
  connect lens (ReduxW f) = ReduxW $ \e w -> (lens %%~ (f e)) w
