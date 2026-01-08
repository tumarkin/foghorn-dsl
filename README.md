# Foghorn.Dsl

The core DSL (Domain-Specific Language) for defining econometric variables, performing calculations, and specifying statistical models in Foghorn.

[![License: PolyForm Noncommercial](https://img.shields.io/badge/license-PolyForm%20Noncommercial-blue)](../LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.6%2B-brightgreen)](https://www.haskell.org/ghc/)
[![Build](https://img.shields.io/badge/build-cabal-informational)](#)

---

## Overview

`foghorn-dsl` provides two interconnected sub-languages:

1. **Algebra Language** - For defining and transforming econometric variables
2. **Language Layer** - For declaring research projects (regressions, observations, tables)

These languages work together: you define variables using the Algebra language, then use them in research projects declared with the Language layer.

---

## Architecture

The DSL is organized into two main subsystems:

### Algebra (`Foghorn.Dsl.Algebra`)

Defines econometric variables and their transformations.

**Core Modules:**
- `Algebra.Language` - Variable operations (aggregations, lags, indicators, transformations)
- `Algebra.Var` - Variable data types (`EVar`, `XVar`)
- `Algebra.Exorcism` - Type erasure for runtime manipulation

**Key Concepts:**

**EVar (Econometric Variable):**
```haskell
data EVar xsource (i :: EconIndices) (d :: EconType)
```
A typed econometric variable with three type parameters:
- `xsource` - The data source module (e.g., `XCoficatVar` for corporate finance)
- `i :: EconIndices` - Index structure (cross-section, time-series, or panel)
- `d :: EconType` - Data type (`Float_`, `Int_`, `Date_`, `Indicator_`, etc.)

**XVar (Existential Variable):**
```haskell
data XVar xsource where
    XVar :: (SingI i, SingI d) => SEconIndices i -> SEconType d -> EVar xsource i d -> XVar xsource
```
Type-erased version of `EVar` that can be stored in collections and manipulated at runtime while maintaining singleton evidence of its original types.

### Language (`Foghorn.Dsl.Language`)

Defines research projects and statistical models.

**Core Modules:**
- `Language` - DSL commands (`estimate_`, `observe_`, `readCsv_`, `table_`)
- `Language.Statement` - Atomic statement types (via Effectful effect)
- `Language.Types` - Core types including `Estimator` type class and `SpecVar`
- `Language.Types.Spec.Estimator` - Estimator type class and common components (RHS, conditions, clustering)
- `Language.Quote` - QuasiQuoter for inline specifications

**Key Concepts:**

**Project Type:**
```haskell
type Project xsource i = Eff '[Stmt xsource i]
```
An Effectful effect for building research projects declaratively. Commands like `estimate_`, `observe_`, and `table_` are monadic actions in this effect.

**Estimator Type Class:**
```haskell
class Estimator xsource i a where
    estimatorVariablesUsed :: a -> Set (SpecVar xsource i)
    estimatorAsStata :: a -> Text
    estimatorName :: a -> Text
```
Defines how to extract variables, generate Stata code, and name any statistical estimator.

**RegHdFe (Regression with High-Dimensional Fixed Effects):**
```haskell
data RegHdFe xsource i = RegHdFe
    { lhs :: !(SpecVar xsource i)        -- Dependent variable
    , rhs :: !(Rhs (SpecVar xsource i))  -- Independent variables
    , conditions :: !(Maybe (Conditions (SpecVar xsource i)))
    , options :: !(Maybe Options)
    , absorb :: !(Set (SpecVar xsource i))  -- Fixed effects
    , vce :: !(Vce (SpecVar xsource i))     -- Clustering
    , name :: !Text                          -- Model name
    }
```

---

## Usage Examples

### Defining Variables with Algebra Language

```haskell
import Foghorn.Base
import Foghorn.Dsl

-- Type alias for variables in this study
type MyVar d = EVar XCoficatVar (Panel DealNumber_ AnnouncementDate_) d

-- Define a cumulative abnormal return
car :: MyVar Float_
car = acquirer $ cumulativeAbnormalReturn (-2, 2) (ReturnOverIndex CrspValueWeighted)

-- Define an indicator variable
stockPayment :: MyVar Indicator_
stockPayment = indicator_ (Sdc.pctPaymentStock > 0)

-- Create a lagged variable
laggedMktCap :: MyVar Float_
laggedMktCap = lag1_ mkCap

-- Compute aggregate statistics
industryMeanSize :: MyVar Float_
industryMeanSize = meanIn_ sizeOfDeal `by1_` [threeDigitSic_ Sdc.acqSicPrimary]

-- Apply transformations
logRelativeSize :: MyVar Float_
logRelativeSize = ln_ (Sdc.dealValue / mkCap)

-- Create conditionally defined variables
firstBid :: MyVar Indicator_
firstBid = indicator_ $ acquirerRolling3YearBidCount == 1
```

### Defining Research Projects with Language Layer

```haskell
import Foghorn.Dsl

-- Define a research project with regressions
regression :: Project XCoficatVar (Panel DealNumber_ AnnouncementDate_) ()
regression = do
    -- Import data
    readCsv_ "data/deals.csv"

    -- Observe summary statistics
    observe_ [car, stockPayment, logRelativeSize, firstBid]

    -- Run baseline regression
    estimate_ $ RegHdFe
        { lhs = car
        , rhs = RhsVars $ Set.fromList [stockPayment, logRelativeSize, firstBid]
        , conditions = Just $ Conditions [stockPayment]
        , options = Nothing
        , absorb = Set.fromList [yearFE, industryFE]
        , vce = Cluster $ Set.fromList [acquirerPermno]
        , name = "baseline"
        }

    -- Run robustness check
    estimate_ $ RegHdFe
        { lhs = car
        , rhs = RhsVars $ Set.fromList [stockPayment, logRelativeSize, firstBid, industryMeanSize]
        , conditions = Nothing
        , options = Nothing
        , absorb = Set.fromList [yearFE]
        , vce = Cluster $ Set.fromList [acquirerPermno, yearFE]
        , name = "robustness"
        }

    -- Output results
    tableDisplay_ $ do
        estimate_ baseline
        estimate_ robustness
```

### Common Algebra Operations

**Time Series Operations:**
```haskell
lag1_ myVar           -- Lag by 1 period
lag2_ myVar           -- Lag by 2 periods
lagN_ 5 myVar         -- Lag by N periods
forward1_ myVar       -- Lead by 1 period
```

**Aggregations:**
```haskell
mean_ myVar                          -- Mean within index groups
meanIn_ myVar `by1_` [groupVar]      -- Mean by specific grouping
max_ myVar                           -- Maximum
standardDeviation_ myVar             -- Standard deviation
percentile_ 0.75 myVar               -- 75th percentile
herfindahl_ marketShare              -- Herfindahl index
```

**Logical and Comparison:**
```haskell
myVar > 0
myVar >= otherVar
myVar == 100
myVar1 && myVar2
not_ condition
myVar `in_` [value1, value2, value3]
```

**Transformations:**
```haskell
ln_ myVar                            -- Natural logarithm
abs_ myVar                           -- Absolute value
square_ myVar                        -- Square
replaceMissingWithZero_ myVar        -- Replace nulls with 0
```

**Date/Time Functions:**
```haskell
extractYear_ dateVar
extractMonth_ dateVar
offsetDate_ 30 dateVar               -- Add 30 days
differenceInDays_ date1 date2
startOfMonth_ dateVar
yearEnd_ panelVar                    -- Select year-end observations
```

**Indicators:**
```haskell
indicator_ (myVar > 0)               -- 1 if condition true, 0 otherwise
indicatorPositive_ myVar             -- 1 if positive, 0 otherwise
```

**Reindexing:**
```haskell
reindex myVar                        -- Change index structure
reindexX myCrossSection myVar        -- Set new cross-section index
reindexT myTimeSeries myVar          -- Set new time-series index
```

### Estimator Manipulation

```haskell
-- Winsorize continuous variables
winsorized :: RegHdFe xsource i -> RegHdFe xsource i
winsorized = winsorizeFloats_ 0.01 0.99

-- Add controls to RHS
withControls :: RegHdFe xsource i -> RegHdFe xsource i
withControls = addToRhs_ [control1, control2, control3]

-- Modify conditions
withSample :: RegHdFe xsource i -> RegHdFe xsource i
withSample = andCondition_ sampleFilter

-- Change fixed effects
withFE :: RegHdFe xsource i -> RegHdFe xsource i
withFE = absorb_ [firmFE, yearFE, industryFE]
```

---

## Key Design Patterns

### Type-Safe Variable Construction

Variables carry their type information at compile time, preventing invalid operations:

```haskell
-- This compiles - both are Float_
result :: MyVar Float_
result = var1 + var2

-- This fails at compile time - can't add Float_ and Int_
-- badResult = floatVar + intVar

-- This compiles - comparison yields Indicator_
condition :: MyVar Indicator_
condition = indicator_ (floatVar > 100)
```

### Singleton Evidence

The DSL uses singletons to maintain type information at runtime through `XVar`:

```haskell
-- EVar has full type information at compile time
myEVar :: EVar XCoficatVar (Panel Permno_ TradingDate_) Float_
myEVar = ...

-- Convert to XVar for runtime storage/manipulation
myXVar :: XVar XCoficatVar
myXVar = exorcise myEVar  -- Type info preserved via singletons

-- Recover type information
xEconType myXVar      -- Returns: Float_
xEconIndices myXVar   -- Returns: Panel Permno_ TradingDate_
```

### Effect-Based Projects

Research projects use Effectful effects for clean separation of declaration and interpretation:

```haskell
-- Declaration (pure)
myProject :: Project XCoficatVar i ()
myProject = do
    estimate_ model1
    estimate_ model2

-- Interpretation happens in transpiler (foghorn-transpile)
-- Can generate SQL, Stata, or other targets
```

---

## Module Exports

The main module `Foghorn.Dsl` re-exports:

**From Algebra:**
- All variable operations from `Algebra.Language`
- Variable types: `EVar`, `XVar`, `ToXVar`
- Type queries: `xEconType`, `xEconIndices`

**From Language:**
- Project DSL from `Language` (`estimate_`, `observe_`, `table_`, etc.)
- Core types from `Language.Types`
- `SpecVar` for named variables in projects
- QuasiQuoters for inline specifications

**From Specification.Estimation:**
- Estimator specifications and transformations

**Utilities:**
- `OffsetDateType` - Date offset types
- `Subsample` - Subsample definitions

---

## Advanced Topics

### Named Intermediate Variables

Store intermediate calculations for reuse:

```haskell
-- Name a variable for intermediate storage
sizeDecile :: MyVar Int_
sizeDecile = named_ "size_decile" $ percentile_ 0.1 dealSize

-- Use in multiple places without recomputation
regression1 = ... sizeDecile ...
regression2 = ... sizeDecile ...
```

### Peer Group Comparisons

Compare observations to their peers:

```haskell
-- Variables for peer comparisons (used in WHEN clauses)
industryMean :: MyVar Float_
industryMean = meanOfPeers_ dealSize `where1_` same_ industryCode

-- Condition: deals larger than industry average
aboveAverage :: MyVar Indicator_
aboveAverage = indicator_ (dealSize > industryMean)
```

### Custom Aggregations

Implement domain-specific aggregations:

```haskell
-- Herfindahl index by custom grouping
industryConcentration :: MyVar Float_
industryConcentration = herfindahlBy_ marketShare [industryCode, fyear]

-- Total of group with custom index
industryTotal :: MyVar Float_
industryTotal = totalOfGroupBy_ dealValue [industryCode, fyear]
```

---

## Compatibility

- GHC: 9.6+
- Build tool: `cabal`
- Dependencies: `foghorn-base`, `effectful-core`, `effectful-th`, `relude`, `singletons`
- OS: Linux/macOS/Windows (standard GHC/cabal environments)

---

## Documentation

In-source Haddock comments document all public functions. To build HTML docs:

```bash
cabal haddock --enable-documentation \
  --haddock-html --haddock-hyperlink-source --haddock-quickjump foghorn-dsl
```

---

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for a detailed history of changes, new features, and bug fixes in each release.

---

## Contributing

Contributions welcome, especially:
- Additional algebraic operations
- New estimator types beyond `RegHdFe`
- Documentation improvements
- Example replications

---

## Maintenance & Support

### Reporting Issues

Found a bug or have a feature request? Please open an issue on the [GitHub issue tracker](https://github.com/tumarkin/foghorn-dsl/issues).

When reporting bugs, include:
- GHC version
- Minimal reproducible example
- Expected vs. actual behavior
- Error messages or stack traces

### Getting Help

- **Issues**: For bug reports and feature requests
- **Discussions**: For questions about usage and design decisions
- **Email**: Contact the maintainer directly for commercial licensing or private inquiries

### Maintenance Status

This project is actively maintained. Security issues and critical bugs are prioritized. Response times may vary depending on the complexity of the issue and maintainer availability.

---

## License

PolyForm Noncommercial 1.0.0. For commercial use, contact **Rob Tumarkin <https://github.com/tumarkin>**.

---

## Maintainer

**Rob Tumarkin** — [@tumarkin](https://github.com/tumarkin)
