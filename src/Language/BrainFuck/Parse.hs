module Language.BrainFuck.Parse
  ( parse
  ) where

import Control.Lens

import Language.BrainFuck.AST

parse :: [Char] -> [Cmd]
parse [] = []
parse ('+':xs) = Add       : parse xs
parse ('-':xs) = Sub       : parse xs
parse ('>':xs) = RightS    : parse xs
parse ('<':xs) = LeftS     : parse xs
parse ('.':xs) = Out       : parse xs
parse (',':xs) = In        : parse xs
parse ('[':xs) = Loop body : parse remaining
 where (body, remaining) = over _1 parse $ break (== ']') xs
parse (_:xs) = parse xs
