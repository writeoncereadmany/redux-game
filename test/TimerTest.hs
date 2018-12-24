{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module TimerTest (htf_thisModulesTests) where

import Control.Lens
import Test.Framework
import Data.Dynamic

import Redux
import Timer
import Graphics.Gloss.Interface.IO.Game

data TestThing = TestThing
  { _toEnqueue :: Maybe (Float, String)
  , _timer :: Timer
  , _fired :: [String]
  }

makeLenses ''TestThing

instance (Typeable a, Show a) => ReduxEvent [ a ]

updateTime :: TimeStep -> TestThing -> Events TestThing
updateTime (TimeStep t) w = case w ^. toEnqueue of
  Nothing     -> return w
  Just (d, e) -> do await d $ fireEvent e
                    return $ toEnqueue .~ Nothing $ w

reduceString :: String -> TestThing -> Events TestThing
reduceString s w = return $ fired %~ (s :) $ w

testRedux :: Redux TestThing
testRedux e w = return w
            >>= connect timer timerRedux e
            >>= focus updateTime e
            >>= focus reduceString e

test_timer_works = do
  let initialTestThing = TestThing (Just (3, "event")) newTimer []
  atTimeZero <- reduxUpdate testRedux 0 initialTestThing

  assertEqual [] (atTimeZero ^. fired)

  atTimeTwo <- reduxUpdate testRedux 2 atTimeZero

  assertEqual [] (atTimeTwo ^. fired)

  atTimeFour <- reduxUpdate testRedux 2 atTimeTwo

  assertEqual ["event"] (atTimeFour ^. fired)
