{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Semigroup.Join
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Data.Semigroup.Join
    ( Join(..)
    )
  where

import Prelude (Enum, Num)

import Data.Eq (Eq)
import Data.Foldable (Foldable)
import Data.Functor (Functor)
import Data.Monoid (Monoid(mempty, mappend))
import Data.Semigroup (Semigroup((<>)))
import Data.Traversable (Traversable)
import Text.Read (Read)
import Text.Show (Show)

import qualified Data.Semilattice.Join as Class
import qualified Data.Semilattice.Lower as Class


newtype Join a = Join {getJoin :: a}
  deriving (Enum, Eq, Foldable, Functor, Num, Read, Show, Traversable)

instance Class.Join a => Semigroup (Join a) where
    Join a1 <> Join a2 = Join (a1 Class.\/ a2)

instance (Class.Join a, Class.Lower a) => Monoid (Join a) where
    mempty = Join Class.lowerBound
    mappend = (<>)
