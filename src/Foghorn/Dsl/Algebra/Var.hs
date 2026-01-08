{- |
Module      : Foghorn.Dsl.Algebra.Var
Description : Econometric variable types and type erasure
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Re-exports core econometric variable types and type erasure functionality.

This module provides the algebraic variable types ('EVar' and 'XVar') used for
defining and manipulating econometric variables. These are the low-level types
that represent variable computations and transformations.

For specification-level variables used in estimators and models, see
"Foghorn.Dsl.Language.Types.SpecVar". 'SpecVar' wraps 'XVar' with additional
metadata like variable names for output tables.

See "Foghorn.Dsl.Algebra.Var.Internal" for detailed documentation on the
variable types and their constructors.
-}

module Foghorn.Dsl.Algebra.Var (
    EVar (..),
    XVar (..),
    ToXVar (..),
    xEconType,
    xEconIndices,
    Subsample,
    Source'Var (..),
    Exorcise (..),
    OffsetDateType (..),
) where

import Foghorn.Dsl.Algebra.Var.Internal
