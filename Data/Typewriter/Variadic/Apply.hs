{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Typewriter.Variadic.Apply where

import Data.Typewriter.Core
import Data.Typewriter.Variadic.Natural

-- TODO: ApplyN has redundant base cases--should either drop the nat, or not
-- insist that the tuple/list/whatever end with Unit

-- | Applies a with at least N arguments to a type-level list of N arguments, 
-- given in the form (x :*: y :*: ... Unit).
class ApplyN n t where
    type Applied n t :: *
    type TupArg n t :: *
    apply :: n -> t -> TupArg n t -> Applied n t

instance ApplyN Z r where
    type Applied Z r = r
    type TupArg Z r = Unit
    apply Zero x Unit = x

instance (ApplyN n r) => ApplyN (S n) (a -> r) where
    type Applied (S n) (a -> r) = Applied n r
    type TupArg (S n) (a -> r) = a :*: TupArg n r
    apply (Succ n) f (x :*: xs) = apply n (f x) xs

-- | Applies a function taking at least N arguments of the same type to the
-- elements of a list. Returns Nothing if the list wasn't long enough, or
-- the result of the function and the unused list items.
class ApplyList n where
    applyList :: n -> Replicate n ((->) x) r -> [x] -> Maybe (r, [x])

instance ApplyList Z where
    applyList Zero z xs = Just (z, xs)

instance (ApplyList n) => ApplyList (S n) where
    applyList _ _ [] = Nothing
    applyList (Succ n) f (x:xs) = applyList n (f x) xs

