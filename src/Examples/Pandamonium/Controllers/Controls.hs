module Examples.Pandamonium.Controllers.Controls where

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Controls.Button
import ReduxGame.Controls.Axis

import Examples.Pandamonium.Entities.Hero

controlRedux :: Redux World
controlRedux = redux
           |*> updateAxis Horizontal
           |*> updateButton Jump
