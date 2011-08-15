{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
----------------------------------------------
-- |
-- 
-- Defines generic sum, product, unit, and empty types. The main purpose of 
-- this is for type hackery that treats generic types specially, to avoid 
-- interfering with normal use of Either, (,), and () in the types being 
-- operated on.

module Data.Typewriter.Types.Algebraic where

import Control.Monad (liftM4)
import Text.Read (Read(..), Lexeme(..), prec, lexP, parens)
import Data.Typewriter.Demote

infixr 1 :*:
infixl 0 :+:
data a :*: b = a :*: b deriving (Eq, Ord, Read, Show)
data a :+: b = InL a | InR b deriving (Eq, Ord, Read, Show)
data Unit = Unit deriving (Eq, Ord, Read, Show)
data Void

-- ugh, freaking bottoms
-- none of these should ever actually get used, but sum types containing Void
-- are legit, so yeah.
instance Show Void where show _ = "(error \"VOID\" :: Void)"
instance Read Void where 
    readPrec = parens
              . prec 10 
              $ liftM4 (\ (Ident "error") (String v) 
                       (Punc "::") (Ident "Void") -> (error v :: Void))
                lexP lexP lexP lexP
instance Eq Void where _ == _ = error "(==) applied to Void"
instance Ord Void where compare _ _ = error "compare applied to Void"



