module ReduxGame.Controls
  ( Button
  , ButtonType (ButtonType)
  , button
  , held
  , onPress
  , onRelease
  , updateButton

  , Axis
  , AxisType (AxisType)
  , OnAxis (Min, Neutral, Max)
  , axis
  , onAxis
  , updateAxis
  ) where

import ReduxGame.Controls.Axis
import ReduxGame.Controls.Button
