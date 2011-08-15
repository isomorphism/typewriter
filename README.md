## What is this?

A dumping ground for all my senseless acts of wonton type hackery. Not for the
faint of heart. In fact, you should probably stop reading right now.

## Why?

Why not?

## What were you *thinking*?

Good question! If you ever find out, let me know.

## Ok. I'd love to gaze into the abyss with you. What've you got?

This is still a work in progress, as I make a half-hearted effort to organize
the bits and pieces of type hackery I had lying around. As of last time I 
bothered to update the README, it has:

- Type classes for working with term proxies for type values, and for kicking
  type-level values downstairs to go slumming amongst the value-level commoners.
- A rehash of Oleg's TypeEq, modernized slightly by replacing `TypeCast` with
  the new `~`.
- Simple type-level lists
- Type-level unsigned binary numbers, i.e., natural numbers as lists of bits 
  instead of grossly inefficient Peano numbers
- Grossly-inefficient Peano numbers, used for setting recusion depth for some
  other stuff:
    - N-fold replication for type constructors (basically a "type-level fold"
      on the Peano numbers)
    - Variadic function application, with type-level lists or regular lists
    - Nat-indexed argument rotation (generalizing `flip`, roughly) and function 
      composition (generalizing `(.)`, `(.).(.)`, &c.)
- Generic algebraic data constructors, used for:
    - A class for cardinality of a type, with conversions to and from `Integer`
	  based on assigning ordinals to the values of the type. Sort of an enhanced
	  version of `Enum`, where enhanced mostly means more ability to do really
	  dumb things.

Coming soon(er than never):

- Expanding the features of some of the above, e.g. implement better math on
  the binary nats.

- More silliness with the generic algebraic constructors. Probably stuff 
  involving partial derivatives, or algebraic refactoring, or isomorphisms
  between types with compatible structure, or something absurd like that. I
  don't even know.

- Some decent generic recursion operators, once I make them work in a way 
  that's a bit less terrible. Don't hold your breath.

- A type-level expression evaluator with some "type" checking and such, which is
  riduclously convoluted and arcane already and it's not even working yet. 
  You could say that the whole thing is *kind* of horrible. 
  
    Ha, ha. Ha.
  
- Some Template Haskell rubbish to derive boilerplate-heavy instances like all
  those `TermProxy` and `Demote` instances all over the place and ughhhh.

Also:

- Suggestions welcome! No idea is too ridiculous to consider!

## This is completely inexcusable. What do you have to say for yourself?

I regret nothing.

