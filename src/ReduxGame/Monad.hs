module ReduxGame.Monad where

doing :: Monad m => (a -> m ()) -> a -> b -> m b
doing action a b = do action a; return b
