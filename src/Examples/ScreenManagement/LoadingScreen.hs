module Examples.ScreenManagement.LoadingScreen where

import Graphics.Gloss

import ReduxGame.Redux
import ReduxGame.Timer
import ReduxGame.Renderer.Renderable

data LoadingScreen = LoadingScreen [ String ]

data AssetLoaded = AssetLoaded String deriving ReduxEvent
data FinishedLoading = FinishedLoading deriving ReduxEvent

instance Renderable LoadingScreen where
  render (LoadingScreen msgs) =
    let indexed = zip [1..] msgs
     in translate (-300) 300 $ color yellow $ Pictures (textline <$> indexed) where
       textline :: (Integer, String) -> Picture
       textline (y, msg) = translate 0.0 (-200.0 * fromInteger y) $ text msg

initialiseLoadingScreen :: Events ()
initialiseLoadingScreen = do
  await 1 $ fireEvent $ AssetLoaded "Graphics loaded"
  await 2 $ fireEvent $ AssetLoaded "Music loaded"
  await 3 $ fireEvent $ AssetLoaded "Levels loaded"
  await 4 $ fireEvent $ FinishedLoading

assetLoaded :: AssetLoaded -> LoadingScreen -> LoadingScreen
assetLoaded (AssetLoaded asset) (LoadingScreen text) = LoadingScreen (text ++ [asset])

loadingScreenRedux :: Redux LoadingScreen
loadingScreenRedux = redux
                 |-> assetLoaded
