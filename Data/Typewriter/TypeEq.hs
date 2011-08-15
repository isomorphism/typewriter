{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Data.Typewriter.TypeEq ( TypeEq(..), TypeNEq(..), (:/~)(..) )where

import Data.Typewriter.Core
import Data.Typewriter.Functions

-- | This is a modified version of Oleg's @TypeEq@. Note that @TypeCast@ is
-- no longer needed, as @~@ serves the same purpose.
class (TypeEq' () x y b) => TypeEq x y b where
    typeEq :: x -> y -> b
    maybeCast :: x -> Maybe y

instance (TypeEq' () x y b) => TypeEq x y b where
    typeEq _ _ = term
    maybeCast x = maybeCast' () x

class (TermProxy b) => TypeEq' q x y b | q x y -> b where
    maybeCast' :: q -> x -> Maybe y

instance (b ~ Yes) => TypeEq' () x x b where
    maybeCast' _ x = Just x

instance (b ~ No) => TypeEq' q x y b where
    maybeCast' _ _ = Nothing


class TypeNEq x y b where
    typeNEq :: x -> y -> b

instance (TermProxy b, TypeEq x y b', Not b' ~ b) => TypeNEq x y b where
    typeNEq x y = tNot $ typeEq x y


class (TypeNEq x y Yes) => x :/~ y
instance (TypeNEq x y Yes) => x :/~ y

