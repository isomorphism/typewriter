{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Typewriter.Variadic.Flip where

import Data.Typewriter.Core

-- TODO: this is really ad-hoc, need something more sensible here
type family Swap t :: *
type instance Swap (f (g x)) = g (f x)

class SwapF f g where
    swap :: f (g x) -> Swap (f (g x))

instance SwapF ((->) a) ((->) b) where
    swap = flip

instance SwapF ((,) a) ((,) b) where
    swap (x, (y, z)) = (y, (x, z))

instance SwapF ((,) a) ((->) b) where
    swap (x, f) = \y -> (x, f y)

instance SwapF ((,) a) Maybe where
    swap (x, m) = fmap ((,) x) m


-- | Rotates the first N+1 arguments of a function. @rotate Z@ is id, 
-- @rotate (S Z)@ is @flip@, @rotate (S (S Z))@ is @flip . (flip .)@, &c.
class RotateN n t where
    type Rotate n t :: *
    rotate :: n -> t -> Rotate n t

instance RotateN Z t where
    type Rotate Z t = t
    rotate Zero x = x

instance ( RotateN n t, (g t') ~ Rotate n t
         , SwapF f g, Functor f
         ) => RotateN (S n) (f t) where
    type Rotate (S n) (f t) = Swap (f (Rotate n t))
    rotate (Succ n) x = swap $ fmap (rotate n) x

