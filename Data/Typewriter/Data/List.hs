{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Typewriter.Data.List where

import Data.Typewriter.Core


type family Head xs :: *
type instance Head (h :*: t) = h

type family Tail xs :: *
type instance Tail (h :*: t) = t

type family Length xs :: *
type instance Length Unit = Z
type instance Length (h :*: t) = S (Length t)

type family Concat xs ys :: *
type instance Concat Unit ys = ys
type instance Concat (x :*: xs) ys = x :*: Concat xs ys

type family Map (f :: * -> *) xs :: *
type instance Map f Unit = Unit
type instance Map f (x :*: xs) = f x :*: Map f xs

type family FoldR (f :: * -> * -> *) z xs :: *
type instance FoldR f z Unit = z
type instance FoldR f z (x :*: xs) = f x (FoldR f z xs)

class List l where
    tConcat :: (List a) => l -> a -> Concat l a
    tLength :: l -> Length l
    tMap    :: (forall x. x -> f x) -> l -> Map f l
    tFoldR  :: (forall x y. x -> y -> f x y) -> z -> l -> FoldR f z l

instance List Unit where
    tConcat Unit ys = ys
    tLength Unit = Zero
    tMap _ Unit = Unit
    tFoldR _ z Unit = z

instance (List t) => List (h :*: t) where
    tConcat (x :*: xs) ys = x :*: tConcat xs ys
    tLength (_ :*: xs) = Succ (tLength xs)
    tMap f (x :*: xs) = f x :*: tMap f xs
    tFoldR f z (x :*: xs) = f x (tFoldR f z xs)


type family Reverse xs :: *
type instance Reverse Unit = Unit
type instance Reverse (x :*: xs) = Concat (Reverse xs) (x :*: Unit)

instance ListReverse Unit where
    tReverse Unit = Unit

class (List l, List (Reverse l)) => ListReverse l where
    tReverse :: l -> Reverse l


type family ZipWith (f :: * -> * -> *) xs ys :: *
type instance ZipWith f xs Unit = Unit
type instance ZipWith f Unit ys = Unit
type instance ZipWith f (x :*: xs) (y :*: ys) = f x y :*: ZipWith f xs ys

class (List xs, List ys) => Zip xs ys where
    tZipWith :: (forall x y. x -> y -> f x y) -> xs -> ys -> ZipWith f xs ys

instance (Zip xs ys) => Zip (x :*: xs) (y :*: ys) where
    tZipWith f (x :*: xs) (y :*: ys) = f x y :*: tZipWith f xs ys

instance (List xs) => Zip (x :*: xs) Unit where
    tZipWith _ _ Unit = Unit

instance (List ys) => Zip Unit (y :*: ys) where
    tZipWith _ Unit _ = Unit

instance Zip Unit Unit where
    tZipWith _ Unit Unit = Unit



