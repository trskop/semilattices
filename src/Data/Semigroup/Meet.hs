{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Semigroup.Meet
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Data.Semigroup.Meet
    ( Meet(..)
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

import qualified Data.Semilattice.Meet as Class
import qualified Data.Semilattice.Upper as Class


newtype Meet a = Meet {getMeet :: a}
  deriving (Enum, Eq, Foldable, Functor, Num, Read, Show, Traversable)

instance Class.Meet a => Semigroup (Meet a) where
    Meet a1 <> Meet a2 = Meet (a1 Class./\ a2)

instance (Class.Meet a, Class.Upper a) => Monoid (Meet a) where
    mempty = Meet Class.upperBound
    mappend = (<>)
