module ReduxGame.Entities.Store where

type EntityId = Integer

data Tagged a = Tagged EntityId a
