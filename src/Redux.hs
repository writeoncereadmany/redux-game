module Redux where

import Data.Dynamic (Typeable)
import Data.ConstrainedDynamic
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer
import Control.Lens
import Data.DList
class (Typeable a) => ReduxEvent a

type DynEvent = ConstrainedDynamic ReduxEvent

instance ReduxEvent Event

data TimeStep = TimeStep Float deriving ReduxEvent

type EventsT m w = WriterT (DList DynEvent) m w
type IOEvents w = EventsT IO w
type Events w = forall m . Monad m => EventsT m w
type Updater a b = forall l . (Functor l, Applicative l) => LensLike' l b a

type Redux' w = DynEvent -> w -> IOEvents w

noOp :: Monad m => a -> b -> m b
noOp = const return

infixl 4 \->

(\->) :: Monad m => (a -> b -> m b) -> (a -> b -> m b) -> (a -> b -> m b)
(\->) f g i w = do w' <- f i w; g i w'

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

handleRemainingEvents' :: Redux' w -> w -> DList DynEvent -> IO w
handleRemainingEvents' f w es = do (w', es') <- runWriterT $ foldM (flip f) w es
                                   case es' of
                                     Nil -> return w'
                                     otherwise -> handleRemainingEvents' f w' es'

reduxDo' :: Redux' w -> w -> Events () -> IO w
reduxDo' r w a = case runWriter a of
  ((), events) -> handleRemainingEvents' r w events


reduxUpdate' :: Redux' w -> Float -> w -> IO w
reduxUpdate' f t w = handleRemainingEvents' f w (singleton $ toDyn $ TimeStep t)

reduxListen' :: Redux' w -> Event -> w -> IO w
reduxListen' f e w = handleRemainingEvents' f w (singleton $ toDyn $ e)


lensing :: (Functor f, Applicative f) => Updater a b -> (i -> a -> f a) -> i -> (b -> f b)
lensing lens f e = lens %%~ (f e)
