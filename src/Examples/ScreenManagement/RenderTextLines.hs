module Examples.ScreenManagement.RenderTextLines where

import Graphics.Gloss

textLines :: [String] -> Float -> Picture
textLines lines spacing = let indexed = zip [1..] lines
   in Pictures (textline <$> indexed) where
     textline :: (Integer, String) -> Picture
     textline (line, msg) = translate 0.0 (-spacing * fromInteger line) $ text msg
