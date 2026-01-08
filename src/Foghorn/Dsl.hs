{- |
Module      : Foghorn.Dsl
Description : Main entry point for the Foghorn econometric DSL
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

This module provides the primary interface to Foghorn's domain-specific language
for econometric research. It re-exports the key components needed for defining
variables, specifying analyses, and building replication studies.

= Overview

The Foghorn DSL is organized into three main layers:

== 1. Algebra Layer

Provides the foundation for defining and transforming economic variables:

* __Operations__ - Combinators for creating derived variables (e.g., 'ln_', 'lag_', 'winsorize_')
* __Variables__ - Core variable types ('EVar', 'XVar', 'SpecVar') with type-level tracking
* __Exorcism__ - Type class instances for extracting variables from complex expressions

See "Foghorn.Dsl.Algebra.Operations" for available variable operations.

== 2. Specification Layer

Defines econometric models and analyses:

* __Estimators__ - Regression specifications:
    * 'Reg' - Basic OLS regressions without absorbed fixed effects
    * 'RegHdFe' - High-dimensional fixed effects regressions using @reghdfe@
* __Language__ - Combinators for building specifications ('estimate_', 'estout_')
* __Variables Used__ - Automatic dependency tracking

See "Foghorn.Dsl.Language" for specification operations,
"Foghorn.Dsl.Estimator.Reg" for basic OLS, and
"Foghorn.Dsl.Estimator.RegHdFe" for fixed effects regressions.

== 3. Variable System

Type-safe variable definitions with compile-time index checking:

* 'EVar' - Type-indexed econometric variables with compile-time tracking
* 'XVar' - Type-erased variables for runtime manipulation
* 'SpecVar' - Named variables for use in specifications

= Usage Example

@
-- Define variables with type-level index information
type MyVar = EVar XCoficatVar (Panel DealNumber_ AnnouncementDate_)

dealValue :: MyVar Float_
dealValue = ...

-- Create derived variables
logDealValue :: MyVar Float_
logDealValue = ln_ dealValue

-- Specify a regression
myRegression :: RegHdFe XCoficatVar (Panel DealNumber_ AnnouncementDate_)
myRegression = RegHdFe
    { lhs = returns
    , rhs = [rhsQ| logDealValue + leverage |]
    , absorb = [absorbQ| industry year |]
    , vce = RobustErrors
    , ...
    }
@

= Module Organization

* "Foghorn.Dsl.Algebra" - Variable operations and transformations
* "Foghorn.Dsl.Language" - Specification DSL and estimator types
* "Foghorn.Dsl.Estimator" - Regression specifications (Reg, RegHdFe)
* "Foghorn.Base" - Foundational types (indices, economic types)

For detailed documentation on specific functionality, see the individual
sub-modules listed in the exports below.
-}

module Foghorn.Dsl (
    -- * Algebra
    -- | Operations for creating and transforming economic variables.
    --
    -- See "Foghorn.Dsl.Algebra.Operations" for available transformations.
    module Foghorn.Dsl.Algebra,
    -- module Foghorn.Dsl.Algebra.Operations,
    -- module Foghorn.Dsl.Algebra.Exorcism,
    -- OffsetDateType (..),
    -- Subsample,

    -- * Specification
    -- | Types and functions for defining econometric models.
    --
    -- See "Foghorn.Dsl.Language" for specification DSL and
    -- "Foghorn.Dsl.Estimator" for regression specifications.
    module Foghorn.Dsl.Language,

    -- ** Estimators
    module Foghorn.Dsl.Estimator,
    -- | Re-exported from "Foghorn.Dsl.Estimator"
    -- Reg (Reg),
    -- reg_,
    -- RegHdFe (RegHdFe),
    -- regHdFe_,

    -- -- * Variables
    -- -- | Core variable types with type-level index tracking.
    -- --
    -- -- * 'EVar' - Economic variables indexed by source and structure
    -- -- * 'XVar' - Cross-module variable references
    -- -- * 'SpecVar' - Variables in specification context (re-exported from Language)
    -- --
    -- -- See "Foghorn.Dsl.Algebra.Var" and "Foghorn.Dsl.Specification.SpecVar"
    -- -- for detailed documentation.
    -- EVar (..),
    -- XVar (..),
    -- ToXVar (..),
    -- xEconType,
    -- xEconIndices,

) where

-- import Foghorn.Dsl.Algebra.Exorcism ()
-- import Foghorn.Dsl.Algebra.Operations
-- import Foghorn.Dsl.Algebra.Var
import Foghorn.Dsl.Algebra
import Foghorn.Dsl.Language
import Foghorn.Dsl.Estimator
