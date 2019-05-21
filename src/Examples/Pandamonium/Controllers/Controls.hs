module Examples.Pandamonium.Controllers.Controls where

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Controls

import Examples.Pandamonium.Labels

controlRedux :: Redux World
controlRedux = redux
           |*> updateAxis Horizontal
           |*> updateButton Jump
