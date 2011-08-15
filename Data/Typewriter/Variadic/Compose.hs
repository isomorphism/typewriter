{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Typewriter.Variadic.Compose where

import Data.Typewriter.Core

-- | Compose at the Nth argument of a function. At @Z@ is the same as @(.)@, at
-- @S Z@ is the same as @(.).(.)@, &c.
class ComposeN n f g where
    type Compose n f g :: *
    compose :: n -> f -> g -> Compose n f g

instance (b ~ b') => ComposeN Z (b -> c) (a -> b') where
    type Compose Z (b -> c) (a -> b') = a -> c
    compose Zero f g = f . g

instance (ComposeN n (b -> c) r) => ComposeN (S n) (b -> c) (a -> r) where
    type Compose (S n) (b -> c) (a -> r) = a -> Compose n (b -> c) r
    compose (Succ nat) f g x = compose nat f (g x)

