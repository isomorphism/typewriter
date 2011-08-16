{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Typewriter.Data.Binary.BitList where

import Data.Typewriter.Core
import Data.Typewriter.Data.Binary.Bit
import Data.Typewriter.Data.List


data END = END deriving (Show)
instance TermProxy END where term = END

data REP b = REP b deriving (Show)
instance (TermProxy b) => TermProxy (REP b) where term = REP term

