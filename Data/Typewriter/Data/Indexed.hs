{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Typewriter.Data.Indexed where

import Data.Typewriter.Core

class Indexed t ix where
    type ElemAt t ix :: *
    (!) :: t -> ix -> ElemAt t ix

