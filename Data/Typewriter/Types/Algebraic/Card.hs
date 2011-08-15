{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Typewriter.Types.Algebraic.Card where

import Control.Applicative ((<$>), Applicative(..))
import Data.Maybe (mapMaybe)
import Data.List (unfoldr)

import Data.Typewriter.Core

-- | Types with a well-defined cardinality.
-- 
-- Sort of an enhanced version of 'Enum', where "enhanced" mostly means the
-- ability to do very stupid things.
--
-- Minimal complete definition: `typeCard`, `toOrdinal`, `fromOrdinal`.

class Card t where
    -- | Find the cardinality of the argument's type. The result should be a
    -- non-negative integer specifying the number of distinct values not 
    -- involving ⊥ inhabiting the type.
    typeCard    :: t -> Integer
    -- | Convert @t@ to an integer between 0 and @typeCard t@ inclusive.
    toOrdinal   :: t -> Integer
    -- | Convert an integer to the equivalent value. Returns `Nothing` if the
    -- integer is larger than the cardinality of the type.
    fromOrdinal :: Integer -> Maybe t
    -- | Enumerates values inhabiting the type.
    enumerate   :: [t]
    
    enumerate = xs
      where xs = mapMaybe fromOrdinal [0 .. typeCard (head xs) - 1]


{- Empty types -}

instance Card Void where
    typeCard _ = 0
    toOrdinal _ = error "toOrdinal applied to Void"
    fromOrdinal _ = Nothing

{- Unit types -}

instance Card () where
    typeCard _ = 1
    toOrdinal () = 0
    fromOrdinal 0 = Just ()
    fromOrdinal _ = Nothing

instance Card Unit where
    typeCard _ = 1
    toOrdinal Unit = 0
    fromOrdinal 0 = Just Unit
    fromOrdinal _ = Nothing    

{- Product types -}

instance (Card a, Card b) => Card (a, b) where
    typeCard _ = typeCard (undefined :: a) * typeCard (undefined :: b)
    toOrdinal (x, y) = toOrdinal x * typeCard y + toOrdinal y
    fromOrdinal n = (,) <$> fromOrdinal (n `rem` sndCard) 
                        <*> fromOrdinal (n `quot` sndCard)
      where sndCard = typeCard (undefined :: b)
    enumerate = (,) <$> enumerate <*> enumerate

instance (Card a, Card b) => Card (a :*: b) where
    typeCard _ = typeCard (undefined :: a) * typeCard (undefined :: b)
    toOrdinal (x :*: y) = toOrdinal x * typeCard y + toOrdinal y
    fromOrdinal n = (:*:) <$> fromOrdinal (n `rem` sndCard) 
                          <*> fromOrdinal (n `quot` sndCard)
      where sndCard = typeCard (undefined :: b)
    enumerate = (:*:) <$> enumerate <*> enumerate

{- Sum types -}

instance (Card a, Card b) => Card (Either a b) where
    typeCard _ = typeCard (undefined :: a) + typeCard (undefined :: b)
    toOrdinal (Left l) = toOrdinal l
    toOrdinal (Right r) = typeCard (undefined :: a) + toOrdinal r
    fromOrdinal n | n < lCard = Left <$> fromOrdinal n
                  | otherwise = Right <$> fromOrdinal (n - lCard)
      where lCard = typeCard (undefined :: a)

instance (Card a, Card b) => Card (a :+: b) where
    typeCard _ = typeCard (undefined :: a) + typeCard (undefined :: b)
    toOrdinal (InL l) = toOrdinal l
    toOrdinal (InR r) = typeCard (undefined :: a) + toOrdinal r
    fromOrdinal n | n < lCard = InL <$> fromOrdinal n
                  | otherwise = InR <$> fromOrdinal (n - lCard)
      where lCard = typeCard (undefined :: a)


{- Functions -}

-- N.B. -- this instance is obviously buggy
--         using this is probably a very bad idea anyway
--         so don't say I didn't warn you!
instance (Card a, Card b) => Card (a -> b) where
    typeCard _ = typeCard (undefined :: b) ^ typeCard (undefined :: a)
    toOrdinal f = sum $ zipWith (\x y -> cdmCard^x * toOrdinal (f y)) [0..] enumerate
      where cdmCard = typeCard (undefined :: b)
    fromOrdinal n | n < 0                  = Nothing
                  | n >= cdmCard ^ domCard = Nothing
                  | otherwise              = Just $ \x -> ys !! fromIntegral (toOrdinal x)
      where ys = expanded
            domCard = typeCard (undefined :: a)
            cdmCard = typeCard (undefined :: b)
            expanded = expandDom n
            expandDom x = let (q, r) = quotRem x cdmCard
                          in case fromOrdinal r of
                                 Nothing -> error "Card instance for function failed -- I warned you, didn't I?"
                                 Just x' -> x' : expandDom q


{- Some other common types -}

-- TODO: use TH to autogenerate these?

instance Card Bool where
    typeCard _ = 2
    toOrdinal False = 0
    toOrdinal True  = 1
    fromOrdinal 0 = Just False
    fromOrdinal 1 = Just True
    fromOrdinal _ = Nothing

instance (Card a) => Card (Maybe a) where
    typeCard _ = 1 + typeCard (undefined :: a)
    toOrdinal Nothing  = 0
    toOrdinal (Just x) = 1 + toOrdinal x
    fromOrdinal 0 = Just Nothing
    fromOrdinal n = Just <$> fromOrdinal (n - 1)


{- Default implementations based on @Enum@ and @Bounded@ -}

defaultTypeCard :: (Bounded a, Enum a) => a -> Integer
defaultTypeCard x = fromIntegral $ 1 + defaultToOrdinal (maxBound `asTypeOf` x)

defaultToOrdinal :: (Bounded a, Enum a) => a -> Integer
defaultToOrdinal x = fromIntegral $ fromEnum x - fromEnum (minBound `asTypeOf` x)

defaultFromOrdinal :: (Bounded a, Enum a) => Integer -> Maybe a
defaultFromOrdinal x | x < 0      = Nothing
                     | x >= xCard = Nothing
                     | otherwise  = Just r 
  where r = toEnum $ (fromIntegral x) + minEnum
        xCard = defaultTypeCard r
        minEnum = fromEnum (minBound `asTypeOf` r)

newtype WrapEnum a = WrapEnum { unwrapEnum :: a } 
    deriving (Eq, Ord, Show, Read)

instance (Enum a, Bounded a) => Card (WrapEnum a) where
    typeCard = defaultTypeCard . unwrapEnum
    toOrdinal = defaultToOrdinal . unwrapEnum
    fromOrdinal n = WrapEnum <$> defaultFromOrdinal n






