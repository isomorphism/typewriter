{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Typewriter.Functions where

import Data.Typewriter.Core


-- basic boolean operations

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


-- get the result type of a function application
type family Apply f x :: *
type instance Apply (a -> b) a = b


-- compare ordering on peano nats
data NatEQ = NatEQ
data NatGT = NatGT
data NatLT = NatLT

instance TermProxy NatEQ where term = NatEQ
instance Demote NatEQ where
    type Unlift NatEQ = Ordering
    unlift NatEQ = EQ

instance TermProxy NatGT where term = NatGT
instance Demote NatGT where
    type Unlift NatGT = Ordering
    unlift NatGT = GT

instance TermProxy NatLT where term = NatLT
instance Demote NatLT where
    type Unlift NatLT = Ordering
    unlift NatLT = LT

type family NatCompare n m :: *
type instance NatCompare Z Z = NatEQ
type instance NatCompare (S n) Z = NatGT
type instance NatCompare Z (S m) = NatLT
type instance NatCompare (S n) (S m) = NatCompare n m

data NatGEQ = NatGEQ
data NatLEQ = NatLEQ
data NatNEQ = NatNEQ

instance TermProxy NatGEQ where term = NatGEQ
instance TermProxy NatLEQ where term = NatLEQ
instance TermProxy NatNEQ where term = NatNEQ

type family NatCompareAs p n m :: *
type instance NatCompareAs NatNEQ n m = Not (NCmpAs NatEQ n m)
type instance NatCompareAs NatLEQ n m = Not (NCmpAs NatGT n m)
type instance NatCompareAs NatGEQ n m = Not (NCmpAs NatLT n m)
type instance NatCompareAs NatEQ n m = NCmpAs NatEQ n m
type instance NatCompareAs NatLT n m = NCmpAs NatLT n m
type instance NatCompareAs NatGT n m = NCmpAs NatGT n m

type family NCmpAs p n m :: *
type instance NCmpAs p (S n) (S m) = NCmpAs p n m
type instance NCmpAs NatGT Z n = No
type instance NCmpAs NatLT n Z = No
type instance NCmpAs NatLT Z (S n) = Yes
type instance NCmpAs NatEQ Z (S n) = No
type instance NCmpAs NatGT (S n) Z = Yes
type instance NCmpAs NatEQ (S n) Z = No
type instance NCmpAs NatEQ Z Z = No




