{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Data.Typewriter.Variadic.Apply where

import Data.Typewriter.Core
import Data.Typewriter.Variadic.Natural

-- | Applies a function taking at least N arguments to a type-level list of 
-- arguments, given in the form (x :*: y :*: ... Unit). Returns a tuple of
-- the result and unused arguments if there were extra, or just the result if
-- the arity matched.
class ApplyN n f xs where
    type AppliedN n f xs :: *
    applyN :: n -> f -> xs -> AppliedN n f xs

instance ApplyN Z r Unit where
    type AppliedN Z r Unit = r
    applyN Zero r Unit = r

instance ApplyN Z r (x :*: xs) where
    type AppliedN Z r (x :*: xs) = (r, x :*: xs)
    applyN Zero r xs = (r, xs)

-- Note the (a ~ x) constraint in the context, with distinct variables in the
-- instance head. The equality constraint lets the types be coerced for terms,
-- while the distinct variables in the head let the instance be chosen even if
-- GHC doesn't know that they match. The end result is that if used on a
-- polymorphic type, the type variables will be forcibly unified, rather
-- than left unknown with a class constraint.
instance (ApplyN n r xs, a ~ x) => ApplyN (S n) (a -> r) (x :*: xs) where
    type AppliedN (S n) (a -> r) (x :*: xs) = AppliedN n r xs
    applyN (Succ nat) f (x :*: xs) = applyN nat (f x) xs


-- | Converts a function taking at least N arguments to a function taking a
-- list of arguments given in the form (x :*: y :*: ... Unit)
class UncurryN n t where
    type UncurriedN n t :: *
    type TupArgs n t :: *
    uncurryN :: n -> t -> TupArgs n t -> UncurriedN n t

instance UncurryN Z (a -> r) where
    type UncurriedN Z (a -> r) = r
    type TupArgs Z (a -> r) = a :*: Unit
    uncurryN Zero f (x :*: Unit) = f x

-- Similar to ApplyN above, an equality constraint is being used here to force
-- unification. In this case, however, the purpose is to allow uncurrying 
-- polymorphic functions to impose the desired arity, e.g., @uncurryN (S Z) id@
-- produces a result of type @(a -> r) :*: r -> r@, matching @uncurry id :: (a -> b, a) -> b@
instance (UncurryN n b, b ~ (b' -> r)) => UncurryN (S n) (a -> b) where
    type UncurriedN (S n) (a -> b) = UncurriedN n b
    type TupArgs (S n) (a -> b) = a :*: TupArgs n b
    uncurryN (Succ nat) f (x :*: xs) = uncurryN nat (f x) xs


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


-- | Given a function taking at least N arguments of the form @(f a)@, where
-- the @f@ is consistent between arguments but the type parameter need not be,
-- applies it N times to a single value polymorphic in the type parameter. 
-- e.g., @applyPoly (S (S Z)) [] (,)@ = @([], []) :: ([a], [b])@
class ApplyPoly n (f :: * -> *) t where
    type AppliedPoly n f t :: *
    applyPoly :: n -> (forall a. f a) -> t -> AppliedPoly n f t

instance ApplyPoly Z f r where
    type AppliedPoly Z f r = r
    applyPoly Zero _ x = x

instance (ApplyPoly n f r, f a ~ t) => ApplyPoly (S n) f (t -> r) where
    type AppliedPoly (S n) f (t -> r) = AppliedPoly n f r
    applyPoly (Succ nat) x f = applyPoly nat x (f x)




