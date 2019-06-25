module ReduxGame.Redux
  ( Redux
  , ReduxEvent
  , BeforeTimeStep (BeforeTimeStep)
  , TimeStep (TimeStep)
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
import ReduxGame.WrappedReduxImpl

data BeforeTimeStep = BeforeTimeStep deriving ReduxEvent
data TimeStep = TimeStep Float deriving ReduxEvent
instance ReduxEvent Event

type Redux w = ReduxW w

fireEvent :: ReduxEvent a => a -> Events ()
fireEvent = tell . singleton . toDyn

reduxUpdate :: Redux w -> Float -> w -> IO w
reduxUpdate f timestep world = reduxDo f world $ do
  fireEvent BeforeTimeStep
  fireEvent $ TimeStep timestep

reduxListen :: Redux w -> Event -> w -> IO w
reduxListen f event world = reduxDo f world (fireEvent event)

onButton :: Char -> Events () -> Event -> a -> Events a
onButton expected action (EventKey (Char actual) _ _ _) a = do
  when (expected == actual) action
  return a
onButton _ _ _ a = return a

infixl 1 |::
(|::) :: Redux w -> Redux w -> Redux w
(|::) = reduxCons

infixl 1 |=>
(|=>) :: ReduxEvent a => Redux w -> (a -> w -> Events w) -> Redux w
(|=>) r f = r |:: (reduxFocus f)

infixl 1 |!>
(|!>) :: ReduxEvent a => Redux w -> (a -> Events ()) -> Redux w
(|!>) redux f = redux |=> doing f where
  doing action a b = do action a; return b

infixl 1 |->
(|->) :: ReduxEvent a => Redux w -> (a -> w -> w) -> Redux w
(|->) redux f = redux |=> (\a w -> return $ f a w)
