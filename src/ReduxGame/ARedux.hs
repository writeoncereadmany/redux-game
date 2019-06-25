module ReduxGame.ARedux where

import Control.Lens
import Control.Monad.Writer
import Data.Typeable
import Data.ConstrainedDynamic
import Data.DList

class (Typeable a) => ReduxEvent a

type DynEvent = ConstrainedDynamic ReduxEvent
type Events w = WriterT (DList DynEvent) IO w
type Updater a b = forall l . (Functor l, Applicative l) => LensLike' l b a

class ARedux r where
  redux :: r a
  reduxDo :: r a -> a -> Events () -> IO a
  reduxCons :: r a -> r a -> r a
  reduxFocus :: (ReduxEvent e, Typeable a) => (e -> a -> Events a) -> r a
  connect :: Updater a b -> r a -> r b
