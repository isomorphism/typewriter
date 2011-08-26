{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Data.Typewriter.Variadic.Natural where

import Data.Typewriter.Core


type family Replicate n (f :: * -> *) x :: *
type instance Replicate Z f x = x
type instance Replicate (S n) f x = f (Replicate n f x)

type family Replicate2 n (f :: (* -> *) -> (* -> *)) (z :: * -> *) :: * -> *
type instance Replicate2 Z f z = z
type instance Replicate2 (S n) f z = f (Replicate2 n f z)


class (Demote n, Unlift n ~ PeanoNat) => Natural n where
    replicateNat  :: n -> (forall a. a -> f a) -> x -> Replicate n f x
    dropNat       :: n -> (forall a. f a -> a) -> Replicate n f x -> x
    replicateNat2 :: n -> (forall g. g x -> f g x) -> z x -> Replicate2 n f z x
    -- ohgod what is even going on here
    fmapNat2      :: n -> (forall g x. f g x -> (x, g x))
                  -> (forall g x. x -> g x -> f g x) 
                  -> (a -> b) -> (z a -> z b)
                  -> Replicate2 n f z a -> Replicate2 n f z b
    -- there's more?!?
    fzipWithNat2  :: n -> (forall g x. f g x -> (x, g x))
                  -> (forall g x. x -> g x -> f g x) 
                  -> (a -> b -> c) -> (z a -> z b -> z c)
                  -> Replicate2 n f z a -> Replicate2 n f z b
                  -> Replicate2 n f z c
    -- auuuuughhhh
    foldrNat2     :: n -> (forall g x. f g x -> (x, g x))
                  -> (a -> r -> r) -> (z a -> r)
                  -> Replicate2 n f z a -> r
    -- IT KEEPS HAPPENING
    unfoldrNat2   :: n -> (forall g x. x -> g x -> f g x)
                  -> (s -> (a, s)) -> (s -> z a)
                  -> s -> Replicate2 n f z a

-- TODO: ok, find a way to clean up the operations here while still preserving 
--       the fully generic approach. would be nice to generalize to things that 
--       don't always produce a single value for each recursive step, also.

fzipNat2 :: (Natural n) => n -> (forall g x. f g x -> (x, g x))
                        -> (forall g x. x -> g x -> f g x) 
                        -> (z a -> z b -> z (a, b))
                        -> Replicate2 n f z a -> Replicate2 n f z b
                        -> Replicate2 n f z (a, b)
fzipNat2 n f1 f2 = fzipWithNat2 n f1 f2 (,)


instance Natural Z where
    replicateNat Zero _ x = x
    dropNat Zero _ x = x
    replicateNat2 Zero f z = z
    fmapNat2 Zero _ _ _ fz x = fz x
    fzipWithNat2 Zero _ _ _ fz x y = fz x y
    foldrNat2 Zero _ _ fz x = fz x
    unfoldrNat2 Zero _ _ fz x = fz x

instance (Natural n) => Natural (S n) where
    replicateNat (Succ nat) f x = f (replicateNat nat f x)
    dropNat (Succ nat) f x = dropNat nat f (f x)
    replicateNat2 (Succ nat) f z = f (replicateNat2 nat f z)
    fmapNat2 (Succ nat) f1 f2 fx fz xs = f2 (fx x) ys
      where (x, xs') = f1 xs
            ys = fmapNat2 nat f1 f2 fx fz xs'
    fzipWithNat2 (Succ nat) f1 f2 fxy fz xs ys = f2 (fxy x y) xys
      where (x, xs') = f1 xs
            (y, ys') = f1 ys
            xys = fzipWithNat2 nat f1 f2 fxy fz xs' ys'
    foldrNat2 (Succ nat) f1 fx fz xs = fx x r
      where (x, xs') = f1 xs
            r = foldrNat2 nat f1 fx fz xs'
    unfoldrNat2 (Succ nat) f1 fx fz x = f1 y (unfoldrNat2 nat f1 fx fz x')
      where (y, x') = fx x
    

type family Add x y :: *
type instance Add x Z = x
type instance Add Z y = y
type instance Add (S x) (S y) = S (S (Add x y))

pnAdd :: (Natural (Add x y), Natural x, Natural y) => x -> y -> Add x y
pnAdd _ _ = term

type family Multiply x y :: *
type instance Multiply x Z = Z
type instance Multiply x (S y) = Add x (Multiply y x)

pnMultiply :: (Natural (Multiply x y), Natural x, Natural y) => x -> y -> Multiply x y
pnMultiply _ _ = term

