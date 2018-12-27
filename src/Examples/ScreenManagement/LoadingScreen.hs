module Examples.ScreenManagement.LoadingScreen where

import Graphics.Gloss

import ReduxGame.Redux
import ReduxGame.Timer
import ReduxGame.Renderer.Renderable

import Examples.ScreenManagement.Transitions
import Examples.ScreenManagement.RenderTextLines

data LoadingScreen = LoadingScreen [ String ]

data AssetLoaded = AssetLoaded String deriving ReduxEvent


instance Renderable LoadingScreen where
  render (LoadingScreen msgs) = translate (-300) 300
                              $ color yellow
                              $ textLines msgs 200

initialiseLoadingScreen :: Events ()
initialiseLoadingScreen = do
  await 1 $ fireEvent $ AssetLoaded "Graphics loaded"
  await 2 $ fireEvent $ AssetLoaded "Music loaded"
  await 3 $ fireEvent $ AssetLoaded "Levels loaded"
  await 4 $ fireEvent $ ToTitleScreen

assetLoaded :: AssetLoaded -> LoadingScreen -> LoadingScreen
assetLoaded (AssetLoaded asset) (LoadingScreen text) = LoadingScreen (text ++ [asset])

loadingScreenRedux :: Redux LoadingScreen
loadingScreenRedux = redux
                 |-> assetLoaded
