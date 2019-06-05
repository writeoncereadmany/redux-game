{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import {-@ HTF_TESTS @-} ReduxTest
import {-@ HTF_TESTS @-} TimerTest
import {-@ HTF_TESTS @-} EntitiesTest

main = htfMain htf_importedTests
