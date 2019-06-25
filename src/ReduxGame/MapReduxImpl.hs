module ReduxGame.MapReduxImpl where

import Data.Typeable
import Data.Dynamic
import Data.Map as M

import ReduxGame.ARedux

typeOfFirstArgument :: forall a b . Typeable a => (a -> b -> Events b) -> TypeRep
typeOfFirstArgument f = let proxy :: Proxy a = Proxy in typeRep proxy

-- unwrap :: Dynamic -> a -> b -> IO b
-- unwrap

data MapRedux a = MapRedux (M.Map TypeRep Dynamic)

instance ARedux MapRedux where
  redux = MapRedux $ M.empty
  reduxDo = undefined
  reduxCons (MapRedux a) (MapRedux b) = undefined
  reduxFocus f = MapRedux (M.singleton (typeOfFirstArgument f) (toDyn f))
  connect lens inner = redux
