{-# LANGUAGE TypeFamilies #-}
module Data.Typewriter.Types.Peano where

import Data.Typewriter.Demote


newtype S nat = Succ nat deriving (Show)
data Z = Zero deriving (Show)

-- just for the sake of having something to demote to
data PeanoNat = Z | S PeanoNat deriving (Eq, Ord, Read, Show)


instance TermProxy Z where term = Zero
instance Demote Z where
    type Unlift Z = PeanoNat
    unlift Zero = Z

instance (TermProxy nat) => TermProxy (S nat) where term = Succ term
instance (Demote nat, Unlift nat ~ PeanoNat) => Demote (S nat) where
    type Unlift (S nat) = PeanoNat
    unlift (Succ n) = S (unlift n)


-- aliases for a few small numbers 
type Zero  = Z
type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three

-- term proxies for the above
zero  = term :: Zero
one   = term :: One
two   = term :: Two
three = term :: Three
four  = term :: Four


