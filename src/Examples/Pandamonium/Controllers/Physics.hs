module Examples.Pandamonium.Controllers.Physics where

import Graphics.Gloss

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Components
import ReduxGame.Collisions

import Examples.Pandamonium.Labels

g = 3000

resetAcceleration :: BeforeTimeStep -> Only Acceleration -> Only Acceleration
resetAcceleration _ _ = Only $ Acceleration (0, 0)

gravity :: TimeStep -> (FeelsGravity, Acceleration) -> Only Acceleration
gravity _ (_, Acceleration (ddx, ddy)) = Only $ Acceleration (ddx, ddy - g)

physicsRedux :: Redux World
physicsRedux = redux
           |$> resetAcceleration
           |$> gravity
           |$> integrate
           |:: collisionRedux
