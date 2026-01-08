{- |
Module      : Foghorn.Dsl.Algebra
Description : Algebraic foundation for econometric variables
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

This module provides the algebraic foundation for Foghorn's econometric DSL,
defining the core types and operations for creating and transforming variables.

= Overview

The Algebra layer is the foundation of Foghorn's type-safe econometric DSL.
It provides:

1. **Variable Types** - Type-indexed representations of econometric data
2. **Operations Language** - Functions and combinators for creating and transforming variables
3. **Type Erasure** - Converting typed variables to runtime-manipulable forms

__Note:__ Direct handling of 'EVar' and 'XVar' types is generally not necessary
for end users. The language automatically manages type-level tracking and type
erasure. Most users only need the operations from "Foghorn.Dsl.Algebra.Operations"
to build variables.

= Architecture

The Algebra layer consists of three main components:

== 1. Variables ("Foghorn.Dsl.Algebra.Var")

Defines the core variable types with type-level tracking:

* 'EVar' - Type-indexed econometric variable
    * Carries source, index structure, and data type at the type level
    * Ensures compile-time correctness of operations
    * Created via DSL operations, not direct construction

* 'XVar' - Type-erased variable with singleton evidence
    * Runtime-manipulable representation
    * Preserves type information through singletons
    * Enables dynamic variable handling

@
-- Type-indexed variable
type MyVar d = EVar XCoficatVar (Panel Permno_ TradingDate_) d

stockPrice :: MyVar Float_
stockPrice = Crsp.dsf.prc

-- Type-erased for runtime manipulation
erasedPrice :: XVar XCoficatVar
erasedPrice = exorcise stockPrice
@

== 2. Operations ("Foghorn.Dsl.Algebra.Operations")

Provides the core operations for variables:

* __Constants__ - 'int_', 'float_', 'date_', 'text_'
* __Logic__ - Boolean operations and comparisons
* __Time Series__ - 'lag1_', 'forward1_', 'lagN_'
* __Aggregations__ - 'mean_', 'max_', 'standardDeviation_'
* __Transformations__ - 'ln_', 'abs_', 'square_'
* __Date\/Time__ - 'extractYear_', 'offsetDate_'
* __Reindexing__ - 'reindex', 'reindexX', 'reindexT'
* __Peer Groups__ - 'meanOfPeers_', 'where1_', 'by1_'
* __Subsampling__ - 'subsample_', 'union_'

@
-- Create derived variables
logPrice = ln_ Crsp.dsf.prc
laggedReturn = lag1_ Crsp.dsf.ret
industryMean = mean_ \@Industry_ Comp.fundamentalData.assets

-- Variables support standard operators
priceChange = Crsp.dsf.prc - lag1_ Crsp.dsf.prc
returnVolatility = standardDeviation_ \@TradingDate_ Crsp.dsf.ret
@

== 3. Type Erasure ("Foghorn.Dsl.Algebra.Exorcism")

Implements the 'Exorcise' type class for converting typed structures
to type-erased equivalents:

* Enables runtime manipulation of typed variables
* Preserves type information through singleton evidence
* Propagates recursively through composite structures

This is primarily used internally by the transpiler when converting
typed specifications to database queries.

= Design Philosophy

The Algebra layer follows these principles:

1. **Type Safety** - Index structures and data types checked at compile time
2. **Composability** - Operations compose naturally with standard Haskell operators
3. **Declarative** - Variables describe "what" not "how"
4. **Transparent** - Type-level information guides but doesn't obstruct

= Usage Patterns

== Creating Variables

Variables are created through three main paths:

1. __Data Sources__ - Import from datasets (e.g., @Crsp.dsf.prc@)
2. __Operations__ - Transform existing variables (e.g., @ln_ price@)
3. __Constants__ - Create literal values (e.g., @float_ 1.0@)

@
-- From data source
price = Crsp.dsf.prc

-- Via transformation
logPrice = ln_ price

-- As constant
threshold = float_ 100.0
@

== Type-Level Tracking

Variables carry their properties at the type level:

@
type StockVar d = EVar XCoficatVar (Panel Permno_ TradingDate_) d

-- Compiler knows this is Float_ with Panel structure
returns :: StockVar Float_
returns = Crsp.dsf.ret

-- Compiler enforces compatible operations
laggedReturns = lag1_ returns  -- ✓ OK - preserves panel structure
@

== Standard Operators

Variables support standard Haskell operators through type class instances:

@
-- Numeric operators
excess = returns - riskFreeRate
scaled = returns * float_ 100.0

-- Comparisons
isPositive = returns > float_ 0.0
isExtreme = abs_ returns > float_ 0.1

-- Logical operators
qualified = isPositive && isLarge
@

= Module Organization

This module re-exports the key components:

* 'EVar', 'XVar' - Core variable types
* 'ToXVar', 'Exorcise' - Type classes for type conversion and erasure
* 'Subsample', 'Source'Var', 'OffsetDateType' - Supporting types
* 'xEconType', 'xEconIndices' - Type information extraction functions
* All operations from "Foghorn.Dsl.Algebra.Operations"

For detailed documentation on specific functionality, see the individual
sub-modules:

* "Foghorn.Dsl.Algebra.Var" - Variable type definitions and type classes
* "Foghorn.Dsl.Algebra.Operations" - Variable operations and transformations
* "Foghorn.Dsl.Algebra.Exorcism" - Type erasure instances

= See Also

* "Foghorn.Dsl.Language" - Specification language for defining analyses
* "Foghorn.Dsl.Estimator" - Regression and estimation specifications
* "Foghorn.Base" - Foundational types (indices, economic types)
-}

module Foghorn.Dsl.Algebra (
    -- * Variables
    -- | Core variable types with type-level index tracking.
    --
    -- * 'EVar' - Type-indexed econometric variables
    -- * 'XVar' - Type-erased variables with singleton evidence
    -- * 'ToXVar' - Type class for converting to XVar
    -- * 'Exorcise' - Type class for type erasure
    -- * 'Subsample' - Type for subsampling operations
    -- * 'Source'Var' - Type for variable sources
    -- * 'OffsetDateType' - Type for date offset operations
    -- * 'xEconType', 'xEconIndices' - Functions to extract type information
    --
    -- See "Foghorn.Dsl.Algebra.Var" for the public interface and
    -- "Foghorn.Dsl.Algebra.Var.Internal" for implementation details.
    module Foghorn.Dsl.Algebra.Var,

    -- * Operations
    -- | Core operations for creating and transforming variables.
    --
    -- See "Foghorn.Dsl.Algebra.Operations" for complete list of operations.
    module Foghorn.Dsl.Algebra.Operations,

    -- * Type Erasure
    -- | Type class instances for converting typed variables to runtime forms.
    --
    -- See "Foghorn.Dsl.Algebra.Exorcism" for implementation details.
    module Foghorn.Dsl.Algebra.Exorcism,
) where

import Foghorn.Dsl.Algebra.Exorcism ()
import Foghorn.Dsl.Algebra.Operations
import Foghorn.Dsl.Algebra.Var
