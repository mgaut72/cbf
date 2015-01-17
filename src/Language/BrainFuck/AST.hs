module Language.BrainFuck.AST where

data Cmd = Add
         | Sub
         | RightS
         | LeftS
         | In
         | Out
         | Loop [Cmd]
         deriving (Show, Eq)
