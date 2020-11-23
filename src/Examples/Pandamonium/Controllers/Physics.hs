module Examples.Pandamonium.Controllers.Physics where

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Components
import ReduxGame.Collisions

import Examples.Pandamonium.Labels

g = 700

resetAcceleration :: BeforeTimeStep -> Acceleration -> Acceleration
resetAcceleration _ _ = Acceleration (0, 0)

gravity :: TimeStep -> (FeelsGravity, Acceleration) -> Acceleration
gravity _ (_, Acceleration (ddx, ddy)) = Acceleration (ddx, ddy - g)

physicsRedux :: Redux World
physicsRedux = redux
           |$> resetAcceleration
           |$> gravity
           |$> applyAcceleration
           |$> applyVelocity
           |:: collisionRedux
