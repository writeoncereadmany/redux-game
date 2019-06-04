module Examples.Pandamonium.Assets.PandaAssets where

import Control.Lens

data PandaAssets = PandaAssets
  {

  }

makeLenses ''PandaAssets

loadAssets :: IO PandaAssets
loadAssets = return PandaAssets
