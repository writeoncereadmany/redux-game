module ReduxGame.Redux
  ( Redux
  , ReduxEvent
  , BeforeTimeStep (BeforeTimeStep)
  , TimeStep (TimeStep)
  , AfterTimeStep (AfterTimeStep)
  , Events
  , fireEvent
  , reduxDo
  , reduxListen
  , reduxUpdate
  , redux
  , connect
  , onButton
  , (|!>)
  , (|=>)
  , (|->)
  , (|::)
  ) where

import Data.Dynamic (Typeable)
import Data.ConstrainedDynamic
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Writer
import Control.Lens
import Data.DList

import ReduxGame.ARedux
import ReduxGame.MapReduxImpl

data BeforeTimeStep = BeforeTimeStep deriving ReduxEvent
data TimeStep = TimeStep Float deriving ReduxEvent
data AfterTimeStep = AfterTimeStep deriving ReduxEvent
instance ReduxEvent Event

type Redux w = MapRedux w

fireEvent :: ReduxEvent a => a -> Events ()
fireEvent = tell . singleton . toDyn

reduxUpdate :: Redux w -> Float -> w -> IO w
reduxUpdate f timestep world = return world
    >>= reduxDo f (fireEvent BeforeTimeStep)
    >>= reduxDo f (fireEvent $ TimeStep timestep)
    >>= reduxDo f (fireEvent AfterTimeStep)

reduxListen :: Redux w -> Event -> w -> IO w
reduxListen f event world = reduxDo f (fireEvent event) world

onButton :: Char -> Events () -> Event -> a -> Events a
onButton expected action (EventKey (Char actual) _ _ _) a = do
  when (expected == actual) action
  return a
onButton _ _ _ a = return a

infixl 1 |::
(|::) :: Redux w -> Redux w -> Redux w
(|::) = reduxCons

infixl 1 |=>
(|=>) :: (ReduxEvent a, Typeable w) => Redux w -> (a -> w -> Events w) -> Redux w
(|=>) r f = r |:: (reduxFocus f)

infixl 1 |!>
(|!>) :: (ReduxEvent a, Typeable w) => Redux w -> (a -> Events ()) -> Redux w
(|!>) redux f = redux |=> doing f where
  doing action a b = do action a; return b

infixl 1 |->
(|->) :: (ReduxEvent a, Typeable w) => Redux w -> (a -> w -> w) -> Redux w
(|->) redux f = redux |=> (\a w -> return $ f a w)
