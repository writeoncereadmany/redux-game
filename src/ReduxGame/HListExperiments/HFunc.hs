module ReduxGame.HListExperiments.HFunc where

import Data.HList

hList :: HList '[String, String, Bool]
hList = "a" .*. "b" .*. True .*. HNil
