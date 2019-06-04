{-# OPTIONS_GHC -F -pgmF htfpp #-}

module SpritesTest (htf_thisModulesTests) where

import ReduxGame.Sprites.SpriteSheet

import Test.Framework


test_extracts_slice_from_top_row = do
  let tilesheet_bounds = Bounds { _left = 0, _top = 0, _width = 100, _height = 100 }
  let sprite_bounds = Bounds { _left = 0, _top = 0, _width = 10, _height = 1 }
  assertEqual (extract_slices tilesheet_bounds sprite_bounds) [(0, 9)]

test_extracts_slice_from_later_row = do
  let tilesheet_bounds = Bounds { _left = 0, _top = 0, _width = 100, _height = 100 }
  let sprite_bounds = Bounds { _left = 10, _top = 10, _width = 10, _height = 1 }
  assertEqual (extract_slices tilesheet_bounds sprite_bounds) [(1010, 1019)]

test_extracts_multiple_slices = do
  let tilesheet_bounds = Bounds { _left = 0, _top = 0, _width = 100, _height = 100 }
  let sprite_bounds = Bounds { _left = 10, _top = 10, _width = 10, _height = 3 }
  assertEqual (extract_slices tilesheet_bounds sprite_bounds) [(1010, 1019), (1110, 1119), (1210, 1219)]
