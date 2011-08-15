{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Typewriter.Functions where

import Data.Typewriter.Core


type family Not x :: *
type instance Not Yes = No
type instance Not No = Yes

type family And x y :: *
type instance And No y = No
type instance And Yes y = y

type family Or x y :: *
type instance Or No y = y
type instance Or Yes y = Yes


tNot :: (TermProxy (Not x)) => x -> Not x
tNot _ = term

tAnd :: (TermProxy (And x y)) => x -> y -> And x y
tAnd _ _ = term

tOr :: (TermProxy (Or x y)) => x -> y -> Or x y
tOr _ _ = term

type family Apply f x :: *
type instance Apply (a -> b) a = b

