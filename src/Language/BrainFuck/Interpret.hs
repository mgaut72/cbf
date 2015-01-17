module Language.BrainFuck.Interpret where

import Control.Lens

import Data.Word (Word8)
import Data.ByteString.Internal (c2w, w2c)

import Language.BrainFuck.AST

type Tape = Zipper Word8

type Zipper a = ([a], [a])

emptyTape = (repeat 0, [])

forward :: Zipper a -> Zipper a
forward (x:xs, ys) = (xs, x:ys)

backward :: Zipper a -> Zipper a
backward (xs, y:ys) = (y:xs, ys)

replace :: Zipper a -> a -> Zipper a
replace (x:xs, ys) a = (a:xs, ys)

peek :: Zipper a -> a
peek (x:xs, _) = x

effect f a = f a >> return a

interpret :: [Cmd] -> IO Tape
interpret cs = interpret' (cs, emptyTape)

interpret' :: ([Cmd], Tape) -> IO Tape
interpret' ([], t) = return t
interpret' state = interpretOne state >>= interpret'

interpretOne :: ([Cmd], Tape) -> IO ([Cmd], Tape)
interpretOne (c:cs, t) = tapeEffect c t >>= \t -> return (cs, t)

tapeEffect c t = case c of
  Add     -> return $ _1 . _head +~ 1 $ t
  Sub     -> return $ _1 . _head -~ 1 $ t
  RightS  -> return . forward $ t
  LeftS   -> return . backward $ t
  Out     -> effect (putChar . w2c . peek) t
  In      -> getChar >>= return . replace t . c2w
  Loop cs -> if peek t == 0
               then return t
               else interpret' (cs, t) >>= tapeEffect c


