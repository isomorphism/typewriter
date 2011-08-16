{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Data.Typewriter.Apply where

import Data.Typewriter.Core

infixl 8 :$>
data f :$> x = f :$> x deriving (Show)
instance (TermProxy f, TermProxy x) => TermProxy (f :$> x) where 
    term = term :$> term

class Apply fx where
    type Eval fx :: *
    eval :: fx -> Eval fx

{- Notes: 

Where type-level programming is concerned, the kind system is not, as it turns
out, very kind at all. Below are some wrappers/proxies/&c. that let the 
evaluator above work with consistent kinds. What this amounts to is:

- For giving kinds other than * to the evaluator, we need a wrapper around them
  that has kind *, with an appropriate Eval implementation that expands the
  desired result when saturated.

- For constructing evaluator expressions by direct application rather than the
  (:$>) type, we need a wrapper that has the desired kind, with an Eval 
  implementation that produces the desired expression to evaluate.

The general limitations of the kind system, in particular the total lack of 
polymorphic kinds, means that the latter needs to be implemented separately for
each desired kind. Fortunately, there's little need for anything beyond two or
three type parameters, and partial application of (:$>) handles the 1-ary case.

-}

-- | A wrapper around a type constructor @f :: * -> *@ and a function that 
-- gives a term-level mapping @forall a. a -> f a@. This serves
data TypeCon f = TypeCon (forall a. a -> f a)
instance Apply (TypeCon f :$> x) where
    type Eval (TypeCon f :$> x) = f x
    eval (TypeCon f :$> x) = f x

-- | A type constructor for "kind-level currying" of functions used by the 
-- evaluator. Given a raw type function that expects something of kind 
-- @* -> * -> *@, then @KCurry2 f@ can be used to produce something that 
-- reduces to the evaluator's version of curried application.
data KCurry2 f x y = KCurry2 f x y
instance (TermProxy f, TermProxy x, TermProxy y) => TermProxy (KCurry2 f x y) where 
    term = KCurry2 term term term

instance (Apply (f :$> x :$> y)) => Apply (KCurry2 f x y) where
    type Eval (KCurry2 f x y) = Eval (f :$> x :$> y)
    eval (KCurry2 f x y) = eval (f :$> x :$> y)

-- | Same idea as @Curry2@ above, extended to take three type parameters.
data KCurry3 f x y z = KCurry3 f x y z
instance (TermProxy f, TermProxy x, TermProxy y, TermProxy z
         ) => TermProxy (KCurry3 f x y z) where 
    term = KCurry3 term term term term

instance (Apply (f :$> x :$> y :$> z)) => Apply (KCurry3 f x y z) where
    type Eval (KCurry3 f x y z) = Eval (f :$> x :$> y :$> z)
    eval (KCurry3 f x y z) = eval (f :$> x :$> y :$> z)


