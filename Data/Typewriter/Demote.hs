{-# LANGUAGE TypeFamilies #-}
module Data.Typewriter.Demote where

{- Notes:

TermProxy reflects type information at the term level, letting type-level 
values be carried through terms in an opaque way, in order to keep things in 
sync so that we can do something with the results.

Demote discards type information, and kicks the type values downstairs to go 
live with their disreputable term-level relatives.

For a dreadfully simple example, see the Data.Typewriter.Types.Boolean module.

-}

class TermProxy t where term :: t

class (TermProxy t) => Demote t where
    type Unlift t :: *
    unlift :: t -> Unlift t


-- This makes explicit the lexical pun of () having type ().
instance TermProxy () where term = ()
instance Demote () where
    type Unlift () = ()
    unlift () = ()

-- everything here distributes over products in the obvious way
instance (TermProxy a, TermProxy b) => TermProxy (a, b) where 
    term = (term, term)
instance (Demote a, Demote b) => Demote (a, b) where
    type Unlift (a, b) = (Unlift a, Unlift b)
    unlift (x, y) = (unlift x, unlift y)

-- N.B. The instances for () and (,) extend in obvious manner to Identity and
--      larger tuples, but that doesn't seem terribly useful.

