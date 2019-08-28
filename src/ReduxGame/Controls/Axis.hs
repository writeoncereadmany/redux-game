module ReduxGame.Controls.Axis
  ( Axis
  , axis
  , onAxis
  , OnAxis (Min, Neutral, Max)
  , AxisType (AxisType)
  , axisPress
  , updateAxis
  ) where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Redux
import ReduxGame.Controls.Button
import ReduxGame.Entities

data OnAxis = Min | Neutral | Max deriving (Eq)

data Axis = Axis
  { _minButton :: Button
  , _maxButton :: Button
  , _onAxis    :: OnAxis
  }

makeLenses ''Axis

data AxisType a = AxisType a Axis deriving Component

axis :: Button -> Button -> Axis
axis min max = Axis min max Neutral

axisPress :: Event -> Axis -> Events Axis
axisPress event axis = return axis
                   >>= minButton %%~ keyPress event
                   >>= maxButton %%~ keyPress event
                   <&> update where
   update :: Axis -> Axis
   update axis = case (held $ axis ^. minButton, held $ axis ^. maxButton) of
     (True, False) -> onAxis .~ Min     $ axis
     (False, True) -> onAxis .~ Max     $ axis
     (_, _)        -> onAxis .~ Neutral $ axis

updateAxis :: forall a . a -> Event -> AxisType a -> Events (AxisType a)
updateAxis _ event (AxisType a axis) = AxisType a <$> axisPress event axis
