module ReduxGame.Redux where

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

type Events w = WriterT (DList DynEvent) IO w
type Updater a b = forall l . (Functor l, Applicative l) => LensLike' l b a

type Redux w = DynEvent -> w -> Events w

focus :: (ReduxEvent a) => (a -> b -> Events b) -> Redux b
focus f e w = case (fromDynamic e) of
  Just x -> f x w
  Nothing -> return w

pureFocus :: (ReduxEvent a) => (a -> b -> b) -> DynEvent -> b -> b
pureFocus f e w = case (fromDynamic e) of
  Just x -> f x w
  Nothing -> w

fireEvent :: ReduxEvent a => a -> Events ()
fireEvent = tell . singleton . toDyn

handleRemainingEvents :: Redux w -> w -> DList DynEvent -> IO w
handleRemainingEvents f world events = do
  (world', events') <- runWriterT $ foldM (flip f) world events
  case events' of
    Nil -> return world'
    otherwise -> handleRemainingEvents f world' events'

reduxDo :: Redux w -> w -> Events () -> IO w
reduxDo r w a = do
  ((), events) <- runWriterT a
  handleRemainingEvents r w events

reduxUpdate :: Redux w -> Float -> w -> IO w
reduxUpdate f timestep world = reduxDo f world (fireEvent $ TimeStep timestep)

reduxListen :: Redux w -> Event -> w -> IO w
reduxListen f event world = reduxDo f world (fireEvent event)

connect :: Updater a b -> (i -> a -> Events a) -> (i -> b -> Events b)
connect lens f e = lens %%~ (f e)

redux :: Redux a
redux = const return

infixl 1 |->

(|->) :: ReduxEvent a => Redux w -> (a -> w -> Events w) -> Redux w
(|->) redux f e w = return w
                >>= redux e
                >>= focus f e

infixl 1 |+>
(|+>) :: Redux w -> Redux w -> Redux w
(|+>) redux1 redux2 e w = return w
                      >>= redux1 e
                      >>= redux2 e
