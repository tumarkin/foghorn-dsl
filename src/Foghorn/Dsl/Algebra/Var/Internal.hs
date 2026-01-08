{-|
Module      : Foghorn.Dsl.Algebra.Var.Internal
Description : Econometric variable data types
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial (see LICENSE file)
Maintainer  : https://github.com/tumarkin
Stability   : experimental
Portability : portable

This module defines the core data types for econometric variables in Foghorn's DSL.

= Overview

Variables in Foghorn carry type-level information about their econometric properties:

* __Data source__ - Which module\/dataset the variable comes from
* __Index structure__ - Cross-sectional, time-series, or panel indices
* __Data type__ - The kind of econometric data (Float_, Int_, Date_, etc.)

The primary types are:

* 'EVar' - Type-indexed econometric variable (compile-time type information)
* 'XVar' - Type-erased variable with singleton evidence (runtime manipulation)

Variables are typically created through operations in "Foghorn.Dsl.Algebra.Operations"
rather than constructed directly.

-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
module Foghorn.Dsl.Algebra.Var.Internal where

import Data.Singletons
import Data.Singletons.Base.TH
import Foghorn.Base
import GHC.TypeLits
import Relude

{- | Econometric variable with type-level indices and data type.

An 'EVar' (econometric variable) represents a variable in Foghorn's declarative
econometric language. Each variable carries type-level information:

* @xsource@ - The source module for this variable (e.g., @XCoficatVar@ for corporate finance)
* @i :: 'EconIndices'@ - The index structure: cross-section, time-series, or panel
* @d :: 'EconType'@ - The econometric data type (e.g., @Float_@, @Int_@, @Date_@)

__Construction:__ 'EVar's should not be constructed directly using these constructors.
Instead, use:

* Data source variables from modules (e.g., @Crsp.dsf.prc@ from CRSP daily stock file)
* Operations from "Foghorn.Dsl.Algebra.Operations" (e.g., @ln_@, @lag1_@, @mean_@)
* Constants via helper functions (e.g., @int_@, @float_@, @date_@)

==== __Example__

@
type MyVar d = EVar XCoficatVar (Panel Permno_ TradingDate_) d

-- These are created via DSL operations, not constructors:
logPrice :: MyVar Float_
logPrice = ln_ Crsp.dsf.prc

laggedReturn :: MyVar Float_
laggedReturn = lag1_ Crsp.dsf.ret
@

-}
data EVar xsource (i :: EconIndices) (d :: EconType) where
    -- | Variable from a data source (dataset or CSV file).
    --
    -- Created by dataset accessor functions or CSV imports.
    Source :: Source'Var xsource i d -> EVar xsource i d

    -- | Constant value (literal).
    --
    -- Use helper functions like @int_@, @float_@, @date_@, @text_@ instead
    -- of constructing directly.
    Constant :: EConst d -> EVar xsource i d

    -- | Arithmetic expression (addition, subtraction, multiplication, division).
    --
    -- Created by numeric operators: @+@, @-@, @*@, @/@
    Algebra :: AlgebraExp (XVar xsource) -> EVar xsource i d

    -- | Logical operation (and, or, not, in).
    --
    -- Created by logical operators: @&&@, @||@, @not_@, @in_@, @notIn_@
    Logic :: LogicOp (XVar xsource) -> EVar xsource i d

    -- | Comparison operation.
    --
    -- Created by comparison operators: @>@, @>=@, @==@, @<=@, @<@, @/=@, @!=@
    Comparison :: ComparisonOp (XVar xsource) -> EVar xsource i d

    -- | Test if variable is null (missing value).
    --
    -- Created by @null_@ function.
    Null :: XVar xsource -> EVar xsource i d

    -- | Test if variable is not null.
    --
    -- Created by negating @null_@ or using @notNull_@.
    NotNull :: XVar xsource -> EVar xsource i d

    -- | Convert boolean to indicator (0/1).
    --
    -- Created by @indicator_@ function.
    Indicator :: XVar xsource -> EVar xsource i d

    -- | Unary operation (abs, ln, log, square, etc.).
    --
    -- Created by unary functions: @abs_@, @ln_@, @log_@, @square_@,
    -- @defaultTo_@, @defaultToZero_@
    Unary :: UnaryOp (XVar xsource) -> XVar xsource -> EVar xsource i d

    -- | Type coercion between econometric types.
    --
    -- Created by coercion functions when needed for type compatibility.
    Coerce :: EconType -> (XVar xsource) -> EVar xsource i d

    -- | Database function (executed within the database engine).
    --
    -- Created by functions like @differenceInDays_@, @extractYear_@, @offsetDate_@,
    -- SIC classification functions, etc.
    DbFunc :: DbFunction (XVar xsource) -> EVar xsource i d

    -- | Conditional expression (if-then-else).
    --
    -- Created by if-then-else expressions or conditional logic in the DSL.
    If :: (XVar xsource) -> (XVar xsource) -> (XVar xsource) -> EVar xsource i d

    -- | Intermediate calculation (named, aggregated, reindexed, or time-series).
    --
    -- Created by functions like @named_@, @lag_@, @mean_@, @reindex@, etc.
    -- These represent computations stored as intermediate results.
    Stored :: StoredVar (XVar xsource) -> EVar xsource i d

{- | Source variable from a dataset or CSV file.

Represents the origin of econometric data, either from a module-provided
dataset or loaded from a CSV file.

__Dataset sources__ are created by data modules (e.g., @foghorn-data-crsp@,
@foghorn-data-sdc@) and accessed through generated accessor functions.

__CSV sources__ allow loading external data with specified index structure
and data type.

Users typically don't construct these directly - they use dataset accessors
or the @readCsv_@ specification command.

-}
data Source'Var xsource (i :: EconIndices) (d :: EconType) where
    -- | Variable from a module-provided dataset.
    --
    -- Created by data modules and accessed through generated functions.
    Source'Dataset :: xsource -> Source'Var xsource i d

    -- | Variable loaded from a CSV file.
    --
    -- Requires explicit index structure and data type specification.
    Source'Csv :: EconIndices -> EconType -> FilePath -> Source'Var xsource i d

{- | Constant values in econometric expressions.

Represents literal values of various econometric types. These are typically
created through helper functions (@int_@, @float_@, @date_@, @text_@, @true_@,
@false_@) rather than constructed directly.

-}
data EConst (d :: EconType) where
    -- | Date constant from year, month, day.
    --
    -- Use @date_@ function: @date_ 2020 1 15@
    CDate :: Int -> Int -> Int -> EConst Date_

    -- | Boolean false constant.
    --
    -- Use @false_@ function.
    CFalse :: EConst Bool_

    -- | Floating-point constant.
    --
    -- Use @float_@ function: @float_ 3.14@
    CFloat :: Double -> EConst Float_

    -- | Fiscal year constant.
    --
    -- Use @int_@ or fiscal year functions.
    CFyear :: Int -> EConst Fyear_

    -- | Integer constant.
    --
    -- Use @int_@ function: @int_ 42@
    CInt :: Int -> EConst Int_

    -- | Null (missing) value of any type.
    --
    -- Use @null_@ function.
    CNull :: EConst d

    -- | Text constant.
    --
    -- Use @text_@ function: @text_ "example"@
    CText :: Text -> EConst Text_

    -- | Boolean true constant.
    --
    -- Use @true_@ function.
    CTrue :: EConst Bool_

-- * Type-Erased Variables (XVar)

{- | Type-erased econometric variable with runtime singleton evidence.

'XVar' wraps an 'EVar' while erasing its type-level index and data type parameters,
but preserving them as runtime singleton values. This allows:

* Storing variables with different indices\/types in the same collection
* Runtime inspection of type information via 'xEconType' and 'xEconIndices'
* Type-safe operations through singleton evidence

The singleton evidence ('SingI' constraints and 'SEconIndices'\/'SEconType' values)
ensures that even though the types are erased, they can be recovered and checked
at runtime when needed.

==== __Example__

@
-- Different typed variables can be stored together as XVar
variables :: [XVar XCoficatVar]
variables = [exorcise logPrice, exorcise stockIndicator, exorcise dealDate]

-- But we can still query their types at runtime
getFloatVars :: [XVar XCoficatVar] -> [XVar XCoficatVar]
getFloatVars = filter (\\v -> xEconType v == Float_)
@

-}
data XVar xsource where
    XVar :: (SingI i, SingI d) => SEconIndices i -> SEconType d -> EVar xsource i d -> XVar xsource

-- | Type class for erasing type-level information while preserving singleton evidence.
--
-- The functional dependency @from -> to@ ensures that for any given source type,
-- there's a unique target type.
class Exorcise from to | from -> to where
    exorcise :: from -> to

-- | Convert 'EVar' to 'XVar' by erasing type parameters but keeping singleton evidence.
--
-- The 'SingI' constraints ensure that singleton values are available for the
-- erased types.
instance (SingI i, SingI d) => Exorcise (EVar xsource i d) (XVar xsource) where
    exorcise = XVar sing sing

-- | Extract the econometric data type from a type-erased variable.
--
-- Uses the singleton evidence stored in 'XVar' to recover the 'EconType'.
--
-- >>> xEconType myXVar
-- Float_
xEconType :: XVar xsource -> EconType
xEconType (XVar _ d _) = fromSing d

-- | Extract the index structure from a type-erased variable.
--
-- Uses the singleton evidence stored in 'XVar' to recover the 'EconIndices'.
--
-- >>> xEconIndices myXVar
-- Panel Permno_ TradingDate_
xEconIndices :: XVar xsource -> EconIndices
xEconIndices (XVar i _ _) = fromSing i

-- | Type class for converting values to 'XVar'.
--
-- The functional dependency @v -> xsource@ means the source type determines
-- the xsource type parameter.
class ToXVar xsource v | v -> xsource where
    toXVar :: v -> XVar xsource

-- | 'XVar' is already an 'XVar', so conversion is identity.
instance ToXVar xsource (XVar xsource) where
    toXVar = id

-- * Type Class Instances

-- ** Show Instances

deriving instance (Show xsource) => Show (EVar xsource i d)
deriving instance (Show xsource) => Show (Source'Var xsource i d)
deriving instance Show (EConst d)
deriving instance (Show xsource) => Show (XVar xsource)

-- ** Eq Instances

deriving instance (Eq xsource) => Eq (EVar xsource i d)
deriving instance (Eq xsource) => Eq (Source'Var xsource i d)
deriving instance Eq (EConst d)

-- | Equality for type-erased variables.
--
-- Two 'XVar's are equal if they have the same indices, data types, and values.
-- Uses singleton equality ('(%~)') to compare type-level indices and data types
-- at runtime. If types don't match, the variables are not equal.
instance (Eq xsource) => Eq (XVar xsource) where
    XVar li ld lv == XVar ri rd rv =
        case li %~ ri of
            Disproved _ -> False
            Proved Refl -> case ld %~ rd of
                Disproved _ -> False
                Proved Refl -> lv == rv

-- ** Ord Instances

deriving instance (Eq xsource, Ord xsource) => Ord (EVar xsource i d)
deriving instance (Ord xsource) => Ord (Source'Var xsource i d)
deriving instance Ord (EConst d)

-- | Ordering for type-erased variables.
--
-- Compares 'XVar's lexicographically: first by index structure, then by data type,
-- then by the underlying variable value. Uses singleton equality ('(%~)') to
-- check if types match before comparing values. If types differ, compares the
-- runtime representations of the types.
instance (Ord xsource) => Ord (XVar xsource) where
    XVar li ld lv <= XVar ri rd rv =
        case li %~ ri of
            Disproved _ -> fromSing li <= fromSing ri
            Proved Refl -> case ld %~ rd of
                Disproved _ -> fromSing ld <= fromSing rd
                Proved Refl -> lv <= rv

-- * Operation Data Types

-- ** Algebraic Operations

{- | Binary arithmetic operations.

Represents the four basic arithmetic operations. Created by numeric operators
on variables: @+@, @-@, @*@, @/@.

The @xvar@ parameter is typically 'XVar' in practice, allowing operations
to combine variables with different types (which will be type-checked during
transpilation).
-}
data AlgebraExp xvar
    = Add xvar xvar        -- ^ Addition (@x + y@)
    | Subtract xvar xvar   -- ^ Subtraction (@x - y@)
    | Multiply xvar xvar   -- ^ Multiplication (@x * y@)
    | Divide xvar xvar     -- ^ Division (@x / y@)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- ** Logical Operations

{- | Logical operations on boolean expressions.

Represents logical connectives and membership testing. Created by logical
operators and functions: @&&@, @||@, @not_@, @in_@, @notIn_@.

The @xvar@ parameter is typically 'XVar' in practice, allowing operations
to combine variables with different types (which will be type-checked during
transpilation).
-}
data LogicOp xvar
    = LogicAnd xvar xvar   -- ^ Logical AND (@x && y@)
    | LogicOr xvar xvar    -- ^ Logical OR (@x || y@)
    | LogicNot xvar        -- ^ Logical NOT (@not_ x@)
    | LogicIn xvar [xvar]  -- ^ Membership test (@x \`in_\` [a, b, c]@)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- ** Comparison Operations

{- | Comparison operations between values.

Represents all six standard comparison operators. Created by comparison
operators: @>@, @>=@, @==@, @<=@, @<@, @/=@, @!=@.

These return boolean (indicator) values when evaluated.

The @xvar@ parameter is typically 'XVar' in practice, allowing operations
to combine variables with different types (which will be type-checked during
transpilation).
-}
data ComparisonOp xvar
    = Greater xvar xvar      -- ^ Greater than (@x > y@)
    | GreaterEqual xvar xvar -- ^ Greater than or equal (@x >= y@)
    | Equal xvar xvar        -- ^ Equal (@x == y@)
    | LessEqual xvar xvar    -- ^ Less than or equal (@x <= y@)
    | Less xvar xvar         -- ^ Less than (@x < y@)
    | NotEqual xvar xvar     -- ^ Not equal (@x /= y@ or @x != y@)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- ** Unary Operations

{- | Unary operations on a single variable.

Represents single-argument mathematical transformations and missing value
handling. Created by unary functions: @abs_@, @ln_@, @log_@, @square_@,
@defaultTo_@, @defaultToZero_@.

Note: Some operations are nullary (like 'ReplaceMissingWithZero') while
others carry additional data (like 'ReplaceMissing' which needs a replacement value).

The @xvar@ parameter is typically 'XVar' in practice, allowing operations
to work with variables of any type.
-}
data UnaryOp xvar
    = Abs                      -- ^ Absolute value (@abs_ x@)
    | Ln                       -- ^ Natural logarithm (@ln_ x@)
    | Log                      -- ^ Base-10 logarithm (@log_ x@)
    | Signum                   -- ^ Sign function (-1, 0, or 1)
    | Square                   -- ^ Square (@square_ x@)
    | ReplaceMissing xvar      -- ^ Replace missing values with given value
    | ReplaceMissingWithZero   -- ^ Replace missing values with zero
    deriving (Show, Eq, Ord)

-- ** Database Functions

{- | Functions executed within the database engine.

These operations are pushed down to the database (SQL) rather than evaluated
in Haskell. This allows for efficient computation on large datasets.

Created by various DSL functions: @differenceInDays_@, @extractYear_@,
@offsetDate_@, SIC classification functions, etc.

The @xvar@ parameter is typically 'XVar' in practice.
-}
data DbFunction xvar
    = DifferenceInDays xvar xvar      -- ^ Days between two dates
    | ExtractYear xvar                -- ^ Extract year from date
    | ExtractMonth xvar               -- ^ Extract month from date
    | ExtractDay xvar                 -- ^ Extract day from date
    | FamaFrench12 xvar               -- ^ Fama-French 12 industry classification
    | FamaFrench48 xvar               -- ^ Fama-French 48 industry classification
    | FromIntegral xvar               -- ^ Convert integer to floating-point
    | HighTechLoughranRitter xvar     -- ^ High-tech classification (Loughran & Ritter)
    | Like xvar Text                  -- ^ SQL LIKE pattern matching
    | OffsetDate OffsetDateType xvar  -- ^ Add time offset to date
    | StartOfMonth xvar               -- ^ First day of month for given date
    | OneDigitSic xvar                -- ^ 1-digit SIC code from full SIC
    | TwoDigitSic xvar                -- ^ 2-digit SIC code from full SIC
    | ThreeDigitSic xvar              -- ^ 3-digit SIC code from full SIC
    | Mod xvar Int                    -- ^ Modulo operation
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | Time offset types for date manipulation.

Specifies how to offset a date by a given amount. Used with 'OffsetDate'
to add or subtract time periods.

Created by @offsetDate_@ function with offset specifications.
-}
data OffsetDateType
    = YearOffset Int   -- ^ Offset by years (e.g., @YearOffset 1@ = add 1 year)
    | WeekOffset Int   -- ^ Offset by weeks
    | MonthOffset Int  -- ^ Offset by months
    | DayOffset Int    -- ^ Offset by days
    deriving (Show, Eq, Ord)

-- * Stored Variables (Intermediate Calculations)

{- | Intermediate calculations that are stored for reuse.

Represents computations that are performed as separate datamanagement
steps instead of forcing the transpiler to perform intricate
calculations. This is important for:

* __Efficiency:__ Avoid recomputing expensive operations
* __Naming:__ Give meaningful names to complex calculations
* __Organization:__ Structure multi-step analyses

There are five categories of stored variables:

1. 'Named' - Explicitly named intermediate values
2. 'Aggregate' - Sample-level aggregations (mean, max, etc.)
3. 'Reindex' - Variables with changed index structure
4. 'Select' - Filtered observations (e.g., year-end only)
5. 'Time' - Time-series operations (lags, leads)

The @xvar@ parameter is typically 'XVar' in practice.
-}
data StoredVar xvar
    = Named (NamedVar xvar)          -- ^ Named intermediate calculation
    | Aggregate (AggregateVar xvar)  -- ^ Sample aggregation
    | Reindex (ReindexVar xvar)      -- ^ Reindexed variable
    | Select (SelectVar xvar)        -- ^ Selected observations
    | Time (TimeVar xvar)            -- ^ Time-series operation
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- ** Named Variables

{- | Named intermediate variable.

Assigns a name to a calculation so it can be stored and reused. The name
appears in generated SQL as a column name.

Created by @named_@ or @namedIn_@ functions.

Fields:

* @name@ - The SQL column\/table name for this calculation
* @sourceVar@ - The variable being named
* @subSample@ - Optional subsample filter (only compute for matching rows)

==== __Example__

@
-- Name a complex calculation for reuse
sizeDecile :: MyVar Int_
sizeDecile = named_ \"size_decile\" $ percentile_ 0.1 dealSize

-- Use the named variable multiple times without recomputation
regression1 = ... sizeDecile ...
regression2 = ... sizeDecile ...
@
-}
data NamedVar xvar = NamedVar
    { name :: !Text
    , sourceVar :: !xvar
    , subSample :: !(Maybe (Subsample xvar))
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- ** Aggregation

{- | Specification for an aggregate calculation (mean, max, total, etc.).

An 'AggregateVar' combines a computation type ('AggregateComp') with a grouping
method ('AggregateMethod') to define how values are aggregated across observations.

==== __Fields__

* @comp@ - The aggregation computation (count, mean, max, etc.)
* @method@ - The grouping method (focal peer-based or by-index grouping)
-}
data AggregateVar xvar = AggregateVar
    { comp :: !(AggregateComp xvar)
    , method :: !(AggregateMethod xvar)
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | The type of aggregation computation to perform.

Specifies what calculation to apply when aggregating values within each group.
-}
data AggregateComp xvar
    = AggCount                         -- ^ Count of observations in each group.
    | AggCompoundReturn xvar           -- ^ Compound return (product of 1+return values).
    | AggHerfindahl xvar               -- ^ Herfindahl index (sum of squared shares).
    | AggMax xvar                      -- ^ Maximum value in each group.
    | AggMean xvar                     -- ^ Arithmetic mean in each group.
    | AggMeanWinsorized Double xvar    -- ^ Winsorized mean (excludes extreme values at specified percentile).
    | AggMedian xvar                   -- ^ Median value in each group.
    | AggPercentile Double xvar        -- ^ Specified percentile (0.0 to 1.0) in each group.
    | AggStDev xvar                    -- ^ Standard deviation in each group.
    | AggTotal xvar                    -- ^ Sum of all values in each group.
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{- | Method for defining aggregation groups.

Determines how observations are grouped together for aggregation: either focal
(aggregate computed separately for each indexed observation) or by-index
(aggregate computed once per index group).
-}
data AggregateMethod xvar
    = AggregateFocal (AggFocal xvar)   -- ^ Compute aggregate for each observation using peer conditions.
    | AggregateBy (AggBy xvar)         -- ^ Compute aggregate once per index group.
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{- | Focal aggregation specification.

Computes the aggregation separately for each indexed observation by identifying
its peers based on conditions. Each observation gets its own aggregate value
computed from its peer group. For example, \"mean of all firms in the same
industry as this firm\" produces a value specific to each observation.

==== __Fields__

* @joinWhere@ - Conditions defining which observations are peers of each focal observation
* @subsample@ - Optional filter to restrict observations before aggregation
* @having@ - Optional group-level filter applied after aggregation
-}
data AggFocal xvar = AggFocal
    { joinWhere :: !(XJoinWhere xvar)
    , subsample :: !(Maybe (Subsample xvar))
    , having :: !(Maybe (XHaving xvar))
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{- | By-index aggregation specification.

Computes the aggregation once for each unique combination of index values,
collapsing the data to the specified index level. For example, \"mean by year\"
produces one value per year that applies to all observations in that year.

==== __Fields__

* @byIndices@ - Pair of optional index variables (cross-section, time-series) defining groups
* @joinWhere@ - Optional additional peer conditions for group membership
* @having@ - Optional group-level filter applied after aggregation
-}
data AggBy xvar = AggBy
    { byIndices :: (Maybe xvar, Maybe xvar)
    , joinWhere :: !(Maybe (XJoinWhere xvar))
    , having :: !(Maybe (XHaving xvar))
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{- | Peer conditions for determining aggregation group membership.

Specifies which observations should be included in the same aggregation group
as the focal observation. Contains a list of peer conditions that define
relationships between observations (e.g., same industry, similar size, within
a date range).

Used in focal aggregations to identify which observations are \"peers\" of each
focal observation for aggregation purposes.
-}
newtype JoinWhere xsource (i :: EconIndices) = JoinWhere [Peer xsource i]
    deriving (Eq, Ord, Show, Semigroup)

{- | Type-erased version of 'JoinWhere'.

@xvar@ is typically 'XVar' in practice.
-}
newtype XJoinWhere xvar = XJoinWhere [XPeer xvar]
    deriving (Eq, Ord, Show, Semigroup, Functor, Foldable, Traversable)

{- | Group-level filter conditions for aggregations.

Specifies conditions that are evaluated after aggregation to filter which
groups are included in the final result. For example, \"only groups where
the count is greater than 10\" or \"only groups where the mean exceeds a
threshold\".

Similar to SQL's HAVING clause, applied after GROUP BY.
-}
newtype Having xsource (i :: EconIndices) = Having [XVar xsource]
    deriving (Eq, Ord, Show, Semigroup)

{- | Type-erased version of 'Having'.

@xvar@ is typically 'XVar' in practice.
-}
newtype XHaving xvar = XHaving [xvar]
    deriving (Eq, Ord, Show, Semigroup, Functor, Foldable, Traversable)

-- ** Grouping (By)

{- | Grouping specification for index-based operations.

Specifies which index variables to group by when performing aggregations or
other operations. Contains a list of variables that define the grouping
structure (e.g., year, industry, firm).
-}
newtype By xsource (i :: EconIndices) = By [XVar xsource]
    deriving (Eq, Ord, Show, Semigroup)

{- | Type-erased version of 'By'.

@xvar@ is typically 'XVar' in practice.
-}
newtype XBy xvar = XBy [xvar]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- ** Reindexing

{- | Variable reindexing specification.

Transforms a variable from one index structure to another. For example,
converting a firm-month variable to a firm-year variable, or changing
the cross-sectional index from deal-level to firm-level.

==== __Fields__

* @method@ - The reindexing method (basic or trading-date-based)
* @xtFrom@ - The source index structure
* @xtTo@ - The target index structure
* @sourceVar@ - The variable to reindex
-}
data ReindexVar xvar = ReindexVar
    { method :: !ReindexMethod
    , xtFrom :: EconIndices
    , xtTo :: EconIndices
    , sourceVar :: !xvar
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | Method for reindexing variables.
-}
data ReindexMethod
    = ReindexBasic                     -- ^ Standard reindexing without special handling.
    | ReindexTradingDateOffset Int     -- ^ Reindex with trading day offset adjustment.
    deriving (Show, Eq, Ord)

-- ** Selecting Observations

{- | Variable with observation selection applied.

Filters the variable to include only observations that meet specific criteria,
such as selecting only year-end observations from a panel dataset.

==== __Fields__

* @method@ - The selection criteria
* @sourceVar@ - The variable to filter
-}
data SelectVar xvar = SelectVar
    { method :: !SelectMethod
    , sourceVar :: !xvar
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | Method for selecting observations.
-}
data SelectMethod
    = YearEnd                          -- ^ Select only year-end observations.
    deriving (Show, Eq, Ord)

-- ** Time Series Operations

{- | Time series transformation of a variable.

Applies time-based operations like lags or leads to shift variable values
forward or backward in time. Commonly used for lagged independent variables
or forward-looking dependent variables.

==== __Fields__

* @op@ - The time series operation (lag or forward)
* @sourceVar@ - The variable to transform
* @subSample@ - Optional filter to restrict which observations are transformed
-}
data TimeVar xvar = TimeVar
    { op :: !TimeOp
    , sourceVar :: !xvar
    , subSample :: !(Maybe (Subsample xvar))
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | Time series operation type.
-}
data TimeOp
    = Lag Natural                      -- ^ Shift values backward in time by N periods.
    | Forward Natural                  -- ^ Shift values forward in time by N periods (lead).
    deriving (Show, Eq, Ord)

-- ** Subsamples

{- | Sample restriction specification for filtering observations.

Defines which subset of observations to include in an operation. Can be
a simple boolean condition, a union of multiple conditions, or a reference
to a precomputed subsample.
-}
data Subsample xvar
    = Subsample xvar                              -- ^ Filter using a boolean variable.
    | Union (Subsample xvar) (Subsample xvar)     -- ^ Union of two subsamples (logical OR).
    | SubsamplePrecomputed Text EconIndices       -- ^ Reference to a precomputed subsample by name.
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- ** Peer Group Operations

{- | Peer relationship specification for focal aggregations.

Defines a relationship between a focal observation and its potential peers.
Combines a peer operation ('PeerOp') with a variable to create a condition
like \"same industry\" or \"within 10% of size\".

Used in focal aggregations to identify which observations should be included
in each observation's peer group.
-}
data Peer xsource (i :: EconIndices) = Peer (PeerOp (XVar xsource)) (XVar xsource)
    deriving (Eq, Ord, Show)

{- | Type-erased version of 'Peer'.

@xvar@ is typically 'XVar' in practice.
-}
data XPeer xvar = XPeer (PeerOp xvar) xvar
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{- | Operation defining a peer relationship.

Specifies how to compare the focal observation to potential peers to determine
if they belong to the same peer group.
-}
data PeerOp xvar
    = PSame                            -- ^ Peers have the same value (e.g., same industry).
    | PDifferent                       -- ^ Peers have different values (e.g., different firm).
    | PLess                            -- ^ Peers have smaller values (e.g., smaller size).
    | PBetween xvar xvar               -- ^ Peers have values within a specified range.
    | PCondition                       -- ^ Peers satisfy a boolean condition.
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{- | Type class for converting values to peer specifications.

Allows implicit conversion from boolean conditions to peer specifications.
-}
class ToPeer xsource i a | a -> i, a -> xsource where
    toPeer :: a -> Peer xsource i

instance ToPeer xsource i (Peer xsource i) where
    toPeer = id

instance
    ( Exorcise (EVar xsource i Bool_) xvar
    , XVar xsource ~ xvar
    )
    => ToPeer xsource i (EVar xsource i Bool_)
    where
    toPeer = Peer PCondition . exorcise

-- * Constant Construction

{- | Type class for converting Haskell values to constant econometric variables.

Enables numeric and other literal values to be used directly in DSL expressions
by automatically converting them to 'EVar' constants. For example, allows writing
@myVar > 0@ instead of @myVar > Constant (CInt 0)@.

==== __Type Parameters__

* @a@ - The Haskell type (e.g., 'Int', 'Double', 'Text')
* @d@ - The target econometric data type (e.g., 'Float_', 'Int_', 'Text_')

==== __Methods__

* 'cvar' - Convert a Haskell value to a constant 'EVar'
-}
class IsConst a d where
    cvar :: a -> EVar xsource i d

-- instance IsConst Double Float_ where
--     cvar = CFloat

instance IsConst Int Float_ where
    cvar = Constant . CFloat . fromIntegral

instance IsConst Int Int_ where
    cvar = Constant . CInt

instance IsConst Int Fyear_ where
    cvar = Constant . CFyear

-- instance IsConst Text Text_ where
--     cvar = CText

-- instance IsString (EVar ksource i Text_) where
--     fromString = CText . fromString

