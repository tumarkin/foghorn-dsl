{- |
Module      : Foghorn.Dsl.Language.Types
Description : Core types for econometric specifications
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Defines the core types and type classes for econometric specifications (regressions,
summary statistics, etc.). The central abstraction is the 'Estimator' type class,
which provides a uniform interface for working with different estimation procedures.

= Module Organization

This module re-exports types and functions from several sub-modules:

== "Foghorn.Dsl.Language.Types.SpecVar"

Named econometric variables:

* 'SpecVar' - Pairs a variable with a display name
* 'specVar' - Constructor for creating named variables

== "Foghorn.Dsl.Language.Statement"

Effectful effect for project statements:

* 'Stmt' - Effect GADT representing atomic project operations
* (Re-exported from Language.Statement for backward compatibility)

== "Foghorn.Dsl.Language.Types.Spec.Core"

Core data structures used in specifications:

* 'Rhs' - Right-hand side variables (including interactions)
* 'Conditions' - Sample filtering conditions
* 'Vce' - Variance-covariance estimation methods
* 'Options' - Additional estimator options

== "Foghorn.Dsl.Language.Types.Spec.Estimator"

The estimator abstraction:

* 'Estimator' - Type class for estimation procedures
* 'Winsorized' - Wrapper that adds winsorization to any estimator

== "Foghorn.Dsl.Language.Types.Spec.Lenses"

Lens type classes and convenience functions for manipulating specifications:

* 'HasName', 'HasLhs', 'HasRhs', 'HasConditions', 'HasVce', 'HasAbsorb' - Lens type classes
* Setter functions - 'name_', 'conditions_', 'vce_', 'absorb_', etc.
* Transformation functions - 'mapMaybeLhsVar', 'mapMaybeRhsVar', 'mapMaybeAbsorbVar'

= Key Concepts

== Estimator Type Class

The 'Estimator' type class provides methods for:

* Extracting variables used in the specification
* Getting a canonical name for the estimator

Code generation for a given target language is handled by backend-specific
type classes in the transpiler (see 'StataTranspilable' in
"Foghorn.Transpile.Stata.Class").

== Component Types

Specifications are built from reusable components:

* 'Rhs' - Right-hand side variables (including interactions)
* 'Conditions' - Filtering conditions (e.g., if statements)
* 'Vce' - Variance-covariance estimation (robust, clustered, etc.)
* 'Options' - Additional estimator-specific options

== Lenses

The module provides lens type classes ('HasName', 'HasLhs', 'HasRhs', etc.)
that allow generic manipulation of estimator components without knowing the
concrete estimator type.

== Winsorization

The 'Winsorized' wrapper adds automatic winsorization to any estimator,
generating code that winsorizes specified variables before estimation.

-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Foghorn.Dsl.Language.Types (
    -- * Named variables
    SpecVar (..),
    specVar,

    -- * Project statements
    Stmt (..),

    -- * Estimators
    Estimator (..),

    -- ** Common estimator components
    Rhs (..),
    Conditions (..),
    Vce (..),
    Options (..),

    -- ** Estimator manipulation
    winsorize_,
    winsorizeOnly_,
    winsorizeFloats_,
    winsorizeByType_,
    Winsorized (..),

    -- * Lenses
    HasName (..),
    HasLhs (..),
    HasRhs (..),
    HasConditions (..),
    HasVce (..),
    HasAbsorb (..),

    -- ** Convenience functions using lenses
    name_,
    lhs_,
    rhs_,
    addToRhs_,
    conditions_,
    andCondition_,
    replaceCondition_,
    vce_,
    absorb_,

    -- ** Transformation with possible failure
    mapMaybeLhsVar,
    mapMaybeRhsVar,
    mapMaybeAbsorbVar,

    -- * Variables used
    getRhsVars,
    getConditionVars,
    getVceVars,
) where

-- Re-export from sub-modules
import Foghorn.Dsl.Language.Types.SpecVar
import Foghorn.Dsl.Language.Statement
import Foghorn.Dsl.Language.Types.Spec.Core
import Foghorn.Dsl.Language.Types.Spec.Estimator
import Foghorn.Dsl.Language.Types.Spec.Lenses
