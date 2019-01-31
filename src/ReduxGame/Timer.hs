module ReduxGame.Timer
  ( elapsed
  , await
  , schedule
  , timerRedux
  , Timer
  , newTimer
  ) where

import Control.Lens
import Data.Dynamic (Typeable)
import Data.ConstrainedDynamic
import Data.DList
import Control.Monad.Writer

import ReduxGame.Redux

data Await = Await Float (Events ()) deriving ReduxEvent
data Pending = Pending Float (Events ()) deriving ReduxEvent

data Timer = Timer
  { _elapsed :: Float
  , _pending :: [ Pending ]
  }

newTimer = Timer 0 []

makeLenses ''Timer

await :: Float -> Events () -> Events ()
await delay action = fireEvent (Await delay action)

schedule :: Float -> Events () -> Events ()
schedule delay action = await delay $ do
  action
  schedule delay action

reduceTimer :: Await -> Timer -> Timer
reduceTimer (Await delay action) timer =
  let newPending = Pending (delay + (timer ^. elapsed)) action
   in pending %~ (newPending :) $ timer

-- if this were a queue, we wouldn't need to iterate over all the events every cycle
-- however, that iteration is cheap and we don't currently expect many timed events
-- to be pending at any given time. If that changes, we should consider changing
-- this data structure then.
updateEvents :: Float -> [ Pending ] -> Events [ Pending ]
updateEvents _ [] = return []
updateEvents elapsed (current@(Pending dueAt event) : rest) = do
  rest' <- updateEvents elapsed rest
  if dueAt <= elapsed
  then do event
          return rest'
  else return (current : rest')

updateTimer :: TimeStep -> Timer -> Events Timer
updateTimer (TimeStep step) (Timer elapsed pending) = do
  let elapsed' = step + elapsed
  pending' <- updateEvents elapsed' pending
  return $ Timer elapsed' pending'

timerRedux :: Redux Timer
timerRedux = redux
         |=> updateTimer
         |-> reduceTimer
