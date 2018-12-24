{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} ReduxTest
import {-@ HTF_TESTS @-} TimerTest

main = htfMain htf_importedTests
