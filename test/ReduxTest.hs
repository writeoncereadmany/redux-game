{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module ReduxTest (htf_thisModulesTests) where

import Control.Lens
import Test.Framework

import Data.Dynamic
import Redux
import Graphics.Gloss.Interface.IO.Game

instance (Typeable a, Show a) => ReduxEvent [ a ]
instance ReduxEvent Float

data TestThing = TestThing
  { _timePassed :: Float
  , _systemEvents :: [ Event ]
  , _timeEvents :: [ Float ]
  , _loggedEvents :: [ String ]
  }

makeLenses ''TestThing

updateTime :: TimeStep -> TestThing -> Events TestThing
updateTime (TimeStep t) w = do
  fireEvent t
  return $ timePassed +~ t $ w

listenEvent :: Event -> TestThing -> Events TestThing
listenEvent e w = do
  fireEvent (show e)
  return $ systemEvents %~ (e :) $ w

reduceNumber :: Float -> TestThing -> IOEvents TestThing
reduceNumber t w = return $ timeEvents %~ (t :) $ w

reduceString :: String -> TestThing -> IOEvents TestThing
reduceString s w = return $ loggedEvents %~ (s :) $ w

testRedux :: Redux' TestThing
testRedux = focusM updateTime
        \-> focusM listenEvent
        \-> focusM reduceNumber
        \-> focusM reduceString

test_update_via_redux' = do
  let initialTestThing = TestThing 0 [] [] []
  updated <- reduxUpdate testRedux 3 initialTestThing

  assertEqual (updated ^. timePassed) 3
  assertEqual (updated ^. timeEvents) [3]

test_listen_via_redux = do
  let initialTestThing = TestThing 0 [] [] []
  updated <- reduxListen testRedux (EventMotion (0, 0)) initialTestThing

  assertEqual (updated ^. systemEvents) [ EventMotion (0, 0) ]
  assertEqual (updated ^. loggedEvents) [ "EventMotion (0.0,0.0)" ]
