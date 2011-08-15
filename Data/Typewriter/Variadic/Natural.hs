{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Data.Typewriter.Variadic.Natural where

import Data.Typewriter.Core


type family Replicate n (f :: * -> *) x :: *
type instance Replicate Z f x = x
type instance Replicate (S n) f x = f (Replicate n f x)


class (Demote n, Unlift n ~ PeanoNat) => Natural n where
    replicateNat :: n -> (forall a. a -> f a) -> x -> Replicate n f x

instance Natural Z where
    replicateNat Zero _ x = x

instance (Natural n) => Natural (S n) where
    replicateNat (Succ nat) f x = f (replicateNat nat f x)


type family Add x y :: *
type instance Add x Z = x
type instance Add Z y = y
type instance Add (S x) (S y) = S (S (Add x y))

pnAdd :: (Natural (Add x y), Natural x, Natural y) => x -> y -> Add x y
pnAdd n1 n2 = term

type family Multiply x y :: *
type instance Multiply x Z = Z
type instance Multiply x (S y) = Add x (Multiply y x)

pnMultiply :: (Natural (Multiply x y), Natural x, Natural y) => x -> y -> Multiply x y
pnMultiply n1 n2 = term

