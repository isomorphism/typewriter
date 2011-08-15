{-# LANGUAGE TypeFamilies #-}
module Data.Typewriter.Types.Boolean where

import Data.Typewriter.Demote

data No = No deriving (Read, Show)
instance TermProxy No where term = No
instance Demote No where
    type Unlift No = Bool
    unlift No = False

data Yes = Yes deriving (Read, Show)
instance TermProxy Yes where term = Yes
instance Demote Yes where
    type Unlift Yes = Bool
    unlift Yes = True

