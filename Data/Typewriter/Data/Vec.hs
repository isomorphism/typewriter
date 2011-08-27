{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typewriter.Data.Vec where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Typewriter.Core
import Data.Typewriter.Types
import Data.Typewriter.Variadic.Natural
import Data.Typewriter.Data.Indexed


-- these are in GADT style because they used to have contexts, but I dropped
-- them later. maybe be better to write them in regular style?
data NilV t where
    NilV :: NilV t

data V v t where
    V :: t -> {-# UNPACK #-} !(v t) -> V v t


{- ** some type-hackery shenanigans going on here! **

the "n" parameter would normally be unused and arbitrary, like a phantom type, 
while "v" would be existential, except that I use both in the equality 
constraint. GHC won't figure out "n" from "v", even though it theoretically 
could, so I rely on anything using the constructor to do the right thing 
explicitly enough for GHC to understand, including determining "n" based on 
the context's type.

more importantly, this preserves the arguments to Replicate2; the 
non-injectivity of type families usually makes it impossible to recover 
arguments automatically. the recursion operations on the Natural class rely on
receiving arguments whose types were made using Replicate/Replicate2, and while 
GHC can match things up for monomorphic types, it's difficult to convince it
that a polymorphic type will always be okay. so the equality hidden in the GADT
here keeps that around for us, by preserving either the obvious match for a
monomorphic type used to create the Vec, or the fact that one of the recursive 
operations on Natural was used to create the Vec.

so basically we just have a not-quite-phantom type along with a constraint that 
lets us use it to calculate the actual type of the contained term, in a way 
that preserves the knowledge that the latter type has a structure compatible 
with a particular type function.

really, what could be simpler?
-}
data Vec n t where
    Vec :: (v ~ Replicate2 n V NilV) => {-# UNPACK #-} !(v t) -> Vec n t


vecSize :: (Natural n) => Vec n t -> n
vecSize _ = term -- TermProxy is pretty handy in cases like this!


-- note that the replicateNat2 call implies the equality constraint Vec needs
mkVec :: (Natural n) => t -> Vec n t
mkVec x = v 
  where -- trick it into conjuring up the correct "n"
        -- not sure why I can't just use `term` here, since it clearly figures
        -- out what "n" should be to satisfy the equality in the GADT
        --     could use scoped type variables I guess...
        v = Vec $ replicateNat2 (vecSize v) (V x) NilV 


-- TODO: clean up this mess, ugh
mapRawVec :: (Natural n) => n -> (a -> b) -> Replicate2 n V NilV a -> Replicate2 n V NilV b
mapRawVec n f = fmapNat2 n (\(V x xs) -> (x, xs)) V f (const NilV)

zipRawVec :: (Natural n) => n -> Replicate2 n V NilV a 
          -> Replicate2 n V NilV b -> Replicate2 n V NilV (a, b)
zipRawVec n v1 v2 = fzipNat2 n (\(V x xs) -> (x, xs)) V (\_ _ -> NilV) v1 v2

zipWithRawVec :: (Natural n) => n -> (a -> b -> c) -> Replicate2 n V NilV a 
              -> Replicate2 n V NilV b -> Replicate2 n V NilV c
zipWithRawVec n f v1 v2 = fzipWithNat2 n (\(V x xs) -> (x, xs)) V f (\_ _ -> NilV) v1 v2

foldrRawVec :: (Natural n) => n -> (a -> r -> r) -> r -> Replicate2 n V NilV a -> r
foldrRawVec n f z = foldrNat2 n (\(V x xs) -> (x, xs)) f (\NilV -> z)

sequenceRawVec :: (Applicative f, Natural n) => n 
               -> Replicate2 n V NilV (f a) -> f (Replicate2 n V NilV a)
sequenceRawVec n = sequenceNat2 n (\(V x xs) -> (x, xs)) V (const (pure NilV))

unfoldrRawVec :: (Natural n) => n -> (r -> (a, r)) -> r -> Replicate2 n V NilV a
unfoldrRawVec n f = unfoldrNat2 n V f (const NilV)



-- hey! listen! er, I mean, stop and think about this one for a moment:
consVec :: (Natural n) => a -> Vec n a -> Vec (S n) a
consVec x (Vec xs) = Vec (V x xs)
{- notice anything? 

this honestly surprised me. the equality constraint obtained from "xs" combined 
with the type signature for "V" is enough to let GHC figure out that the result 
matches Replicate2 for "n"'s successor! despite the fact that this function is
polymorphic in "n", so there's no way it could actually calculate a specific
type at compile time. after all the hoops I sometimes jump through to trick GHC 
into understanding what I'm telling it to do, it's really, really neat to 
see it add 1 and 1 to get 2 on its own.

well. more like "add 1 and N to get N+1" actually.
-}


-- on to some boringly simple functions on stuff wrapped in Vec. note the 
-- consistent lack of pesky constraints in the context!
headVec :: (Natural n) => Vec (S n) a -> a
headVec (Vec (V x _)) = x

tailVec :: (Natural n) => Vec (S n) a -> Vec n a
tailVec (Vec (V _ xs)) = Vec xs

mapVec :: (Natural n) => (a -> b) -> Vec n a -> Vec n b
mapVec f vec@(Vec v) = Vec $ mapRawVec (vecSize vec) f v

zipVec :: (Natural n) => Vec n a -> Vec n b -> Vec n (a, b)
zipVec vec@(Vec v1) (Vec v2) = Vec $ zipRawVec (vecSize vec) v1 v2

zipWithVec :: (Natural n) => (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWithVec f vec@(Vec v1) (Vec v2) = Vec $ zipWithRawVec (vecSize vec) f v1 v2

sequenceVec :: (Applicative f, Natural n) => Vec n (f a) -> f (Vec n a)
sequenceVec vec@(Vec v) = Vec <$> sequenceRawVec (vecSize vec) v

foldrVec :: (Natural n) => (a -> r -> r) -> r -> Vec n a -> r
foldrVec f z vec@(Vec v) = foldrRawVec (vecSize vec) f z v

-- N.B. there's no Maybe on the result, because the Vec size already determines
-- how far the unfold should run.
unfoldrVec :: (Natural n) => (s -> (a, s)) -> s -> Vec n a
unfoldrVec f z = v
  where v = Vec $ unfoldrRawVec (vecSize v) f z

-- as you'd expect, this will produce however many elements the Vec holds,
-- rather than the infinite sequence given by Data.List.iterate
iterateVec :: (Natural n) => (a -> a) -> a -> Vec n a
iterateVec f = unfoldrVec (\x -> (f x, f x))



instance (Natural n) => Functor (Vec n) where 
    fmap = mapVec

instance (Natural n) => Applicative (Vec n) where
    pure = mkVec
    f <*> x = zipWithVec ($) f x

instance (Natural n) => Foldable (Vec n) where
    foldr = foldrVec

instance (Natural n) => Traversable (Vec n) where
    sequenceA = sequenceVec

-- this is not a reasonable instance of Show in any possible way
-- it's just here as a temporary hack to make it easier to look at a Vec
-- from the GHCi prompt. meh. :T
instance (Show a, Natural n) => Show (Vec n a) where
    show = show . toList



