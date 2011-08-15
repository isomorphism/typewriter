{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Typewriter.Data.Nat where

import Data.Typewriter.Core
import Data.Typewriter.Data.List

data O = O deriving (Show)
instance TermProxy O where term = O

data X = X deriving (Show)
instance TermProxy X where term = X

class Natural n where
    toInt :: n -> Int

instance Natural Unit where
    toInt Unit = 0

instance (Natural nat) => Natural (X :*: nat) where
    toInt (X :*: nat) = 1 + 2 * toInt nat

instance (Natural nat) => Natural (O :*: nat) where
    toInt (O :*: nat) = 2 * toInt nat


type family CarryBit a b c :: *
type instance CarryBit O O c = O
type instance CarryBit O b O = O
type instance CarryBit a O O = O
type instance CarryBit X X c = X
type instance CarryBit X b X = X
type instance CarryBit a X X = X

type family SumBit a b c :: *
type instance SumBit O O c = c
type instance SumBit O b O = b
type instance SumBit a O O = a
type instance SumBit X X c = c
type instance SumBit X b X = b
type instance SumBit a X X = a

carryBit :: (TermProxy (CarryBit a b c)) => a -> b -> c -> CarryBit a b c
carryBit _ _ _ = term

sumBit :: (TermProxy (SumBit a b c)) => a -> b -> c -> SumBit a b c
sumBit _ _ _ = term

type family Adder c n m :: *
type instance Adder O ns Unit = ns
type instance Adder O Unit ms = ms
type instance Adder X ns Unit = Adder O (X :*: Unit) ns
type instance Adder X Unit ms = Adder O (X :*: Unit) ms
type instance Adder c (n :*: ns) (m :*: ms) = SumBit c n m 
                                          :*: Adder (CarryBit c n m) ns ms

class BitAdder c n m where
    bitAdder :: c -> n -> m -> Adder c n m

instance BitAdder O Unit Unit where
    bitAdder O Unit Unit = Unit

instance BitAdder O (n :*: ns) Unit where
    bitAdder O ns Unit = ns

instance BitAdder O Unit (m :*: ms) where
    bitAdder O Unit ms = ms

instance BitAdder X Unit Unit where
    bitAdder X Unit Unit = X :*: Unit

instance ( BitAdder O (X :*: Unit) (n :*: ns)
         ) => BitAdder X (n :*: ns) Unit where
    bitAdder X ns Unit = bitAdder O (X :*: Unit) ns

instance ( BitAdder O (X :*: Unit) (m :*: ms) 
         ) => BitAdder X Unit (m :*: ms) where
    bitAdder X Unit ms = bitAdder O (X :*: Unit) ms

instance ( TermProxy (SumBit c n m)
         , TermProxy (CarryBit c n m)
         , BitAdder (CarryBit c n m) ns ms
         ) => BitAdder c (n :*: ns) (m :*: ms) where
    bitAdder c (n :*: ns) (m :*: ms) = sumBit c n m 
                                   :*: bitAdder (carryBit c n m) ns ms
                                                    
nat0 = Unit
nat1 = X :*: nat0
nat2 = O :*: nat1
nat3 = X :*: nat1
nat4 = O :*: nat2
nat5 = X :*: nat2
nat6 = O :*: nat3
nat7 = X :*: nat3
nat8 = O :*: nat4
nat9 = X :*: nat4

type family Plus n m :: *
type instance Plus n m = Adder O n m

plus :: (BitAdder O n m) => n -> m -> Plus n m
plus n m = bitAdder O n m



