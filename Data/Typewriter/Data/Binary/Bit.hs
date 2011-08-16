{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Typewriter.Data.Binary.Bit where

import Data.Typewriter.Core
import Data.Typewriter.Apply

data O = O deriving (Show)
instance TermProxy O where term = O
instance Demote O where
    type Unlift O = Int
    unlift O = 0

data X = X deriving (Show)
instance TermProxy X where term = X
instance Demote X where
    type Unlift X = Int
    unlift X = 1


data NOT = NOT deriving (Show)
instance TermProxy NOT where term = NOT
instance Apply (NOT :$> O) where
    type Eval (NOT :$> O) = X
    eval (NOT :$> O) = X
instance Apply (NOT :$> X) where
    type Eval (NOT :$> X) = O
    eval (NOT :$> X) = O

data AND = AND deriving (Show)
instance TermProxy AND where term = AND
instance Apply (AND :$> X :$> b) where
    type Eval (AND :$> X :$> b) = b
    eval (AND :$> X :$> b) = b
instance Apply (AND :$> O :$> b) where
    type Eval (AND :$> O :$> b) = O
    eval (AND :$> O :$> b) = O


data OR = OR deriving (Show)
instance TermProxy OR where term = OR
instance Apply (OR :$> O :$> b) where
    type Eval (OR :$> O :$> b) = b
    eval (OR :$> O :$> b) = b
instance Apply (OR :$> X :$> b) where
    type Eval (OR :$> X :$> b) = X
    eval (OR :$> X :$> b) = X

data XOR = XOR deriving (Show)
instance TermProxy XOR where term = XOR
instance Apply (XOR :$> O :$> b) where
    type Eval (XOR :$> O :$> b) = b
    eval (XOR :$> O :$> b) = b
instance Apply (XOR :$> b :$> O) where
    type Eval (XOR :$> b :$> O) = b
    eval (XOR :$> b :$> O) = b
instance Apply (XOR :$> X :$> X) where
    type Eval (XOR :$> X :$> X) = O
    eval (XOR :$> X :$> X) = O




