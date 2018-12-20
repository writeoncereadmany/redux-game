module Redux where

import Data.Dynamic (Typeable)
import Data.ConstrainedDynamic
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer
import Control.Lens
import Data.DList

class (Typeable a) => ReduxEvent a

type DynEvent = ConstrainedDynamic ReduxEvent
type EventsT m w = WriterT (DList DynEvent) m w
type IOEvents w = EventsT IO w
type Events w = forall m . Monad m => EventsT m w
type Updater a b = forall l . (Functor l, Applicative l) => LensLike' l b a

data Redux w = Redux
  { reducer :: DynEvent -> w -> IOEvents w
  , updater ::  Float -> w -> Events w
  , listener :: Event -> w -> Events w
  }

noOp :: Monad m => a -> b -> m b
noOp = const return

infixl 4 \->

(\->) :: Monad m => (a -> b -> m b) -> (a -> b -> m b) -> (a -> b -> m b)
(\->) f g i w = do w' <- f i w; g i w'

instance Semigroup (Redux w) where
  (<>) a b = Redux
    { reducer = reducer a \-> reducer b
    , updater = updater a \-> updater b
    , listener = listener a \-> listener b
    }

instance Monoid (Redux w) where
  mempty = Redux { reducer  = noOp, updater  = noOp, listener = noOp }

focusM :: (ReduxEvent a, Monad m) => (a -> b -> m b) -> DynEvent -> b -> m b
focusM f e w = case (fromDynamic e) of
  Just x -> f x w
  Nothing -> return w

focus :: (ReduxEvent a) => (a -> b -> b) -> DynEvent -> b -> b
focus f e w = case (fromDynamic e) of
  Just x -> f x w
  Nothing -> w

fireEvent :: ReduxEvent a => a -> Events ()
fireEvent = tell . singleton . toDyn

handleRemainingEvents :: Redux w -> w -> DList DynEvent -> IO w
handleRemainingEvents r w e = do (world, events) <- runWriterT $ foldM (flip $ reducer r) w e
                                 case events of
                                   Nil -> return world
                                   otherwise -> handleRemainingEvents r world events

reduxDo :: Redux w -> w -> Events () -> IO w
reduxDo r w a = case runWriter a of
  ((), events) -> handleRemainingEvents r w events

reduxListen :: Redux w -> Event -> w -> IO w
reduxListen r e w = case runWriter $ listener r e w of
  (world, events) -> handleRemainingEvents r world events

reduxUpdate :: Redux w -> Float -> w -> IO w
reduxUpdate r t w = case runWriter $ updater r t w of
  (world, events) -> handleRemainingEvents r world events

lensing :: (Functor f, Applicative f) => Updater a b -> (i -> a -> f a) -> i -> (b -> f b)
lensing lens f e = lens %%~ (f e)

connect :: Redux a -> Updater a b -> Redux b
connect redux lens = Redux
  { reducer  = lensing lens (reducer redux)
  , updater  = lensing lens (updater redux)
  , listener = lensing lens (listener redux)
  }
