{- |
Module      : Foghorn.Dsl.Algebra.Operations
Description : Core operations for econometric variables
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial (see LICENSE file)
Maintainer  : https://github.com/tumarkin
Stability   : experimental
Portability : portable

Operations for constructing and transforming econometric variables.

This module provides the core operations for building 'EVar' variables from
data sources, constants, and transformations.

__Note on operator conflicts:__ This module exports comparison and logical
operators (@>@, @>=@, @==@, @<=@, @<@, @/=@, @&&@, @||@) that conflict with
Prelude operators. Dot-suffixed versions (@>.@, @>=.@, @==.@, @<=.@, @<.@, @/=.@,
@&&.@, @||.@) are provided to avoid conflicts.

To use DSL operators without qualification, hide the Prelude versions:

@
import Prelude hiding ((&&), (||), (>), (>=), (==), (<=), (<), (/=))
@

Alternatively, to use Prelude operators and DSL dot-suffixed versions, hide the DSL operators:

@
import Foghorn.Dsl.Algebra.Operations hiding ((&&), (||), (>), (>=), (==), (<=), (<), (/=), (!=))
@

It includes functions for:

* __Constants__ - Creating literal values (@int_@, @float_@, @date_@, @text_@)
* __Logic__ - Boolean operations (@&&@, @||@, @not_@) and comparisons (@>@, @==@, etc.)
* __Time Series__ - Lags and leads (@lag1_@, @forward1_@, @lagN_@)
* __Aggregations__ - Summary statistics (@mean_@, @max_@, @standardDeviation_@, @herfindahl_@)
* __Transformations__ - Mathematical operations (@ln_@, @abs_@, @square_@)
* __Indicators__ - Boolean to numeric conversion (@indicator_@)
* __Date/Time__ - Date extraction and manipulation (@extractYear_@, @offsetDate_@)
* __Reindexing__ - Changing index structures (@reindex@, @reindexX@, @reindexT@)
* __Peer Groups__ - Focal aggregations (@meanOfPeers_@, @where1_@, @by1_@)
* __Subsampling__ - Filtering observations (@subsample_@, @union_@)

__Note:__ Variables support standard numeric operators (@+@, @-@, @*@, @/@) through
type class instances defined elsewhere. This module focuses on domain-specific
econometric operations.

==== __Naming Convention__

Most functions follow the pattern @operation_@ with a dot suffix to
distinguish them from Haskell Prelude functions and to indicate they're DSL operations.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Foghorn.Dsl.Algebra.Operations (
    -- * Constants
    int_, 
    float_,
    date_,
    text_,

    -- * Undefined
    undefined_,
    
    -- * Logic
    not_,
    true_,
    false_,
    null_,

    -- ** Logical operators
    (&&),
    (&&.),
    and_,
    (||),
    (||.),
    or_,

    -- ** Comparison
    (>),
    (>.),
    (>=),
    (>=.),
    (==),
    (==.),
    (<=),
    (<=.),
    (<),
    (<.),
    (/=),
    (/=.),
    (!=),
    (!=.),

    -- ** Algebraic convenience
    negate_,

    -- ** Membership
    in_,
    notIn_,

    -- ** Peer
    same_,
    different_,
    less_,
    between_,

    -- * Functions

    -- ** Date time
    differenceInDays_,

    -- ** Naming intermediate variables
    named_,
    namedIn_,

    -- ** Time series
    lagN_,
    lag_,
    lag1_,
    lag2_,
    lag3_,
    lag4_,
    lag5_,
    lagNIn_,
    forwardN_,
    forward_,
    forward1_,
    forward2_,
    forward3_,
    forward4_,
    forward5_,
    forwardNIn_,

    -- ** Unary
    abs_,
    ln_,
    log_,
    square_,
    defaultToZero_,
    defaultTo_,
    withDefault_,

    -- ** Indicators
    indicator_,
    indicatorPositive_,

    -- ** Aggregate
    count_,
    countIn_,
    max_,
    maxIn_,
    mean_,
    meanIn_,
    meanWinsorized_,
    meanWinsorizedIn_,
    herfindahl_,
    herfindahlIn_,
    percentile_,
    percentileIn_,
    compoundReturn_,
    compoundReturnIn_,
    standardDeviation_,
    standardDeviationIn_,
    totalOfGroup_,
    totalOfGroupIn_,

    -- ** Aggregation of peers
    meanOfPeers_,
    meanOfPeersIn_,
    medianOfPeers_,
    medianOfPeersIn_,

    -- *** Aggregate by
    herfindahlBy_,
    totalOfGroupBy_,

    -- ** Selecting specific observations
    yearEnd_,

    -- *** Conditions
    where1_,
    ToJoinWhere (..),
    by1_,
    ToBy (..),

    -- ** Changing of indexes
    Reindex,
    reindexX,
    reindexT,
    reindex,

    -- *** Date/Time
    extractYear_,
    extractMonth_,
    extractDay_,
    offsetDate_,
    startOfMonth_,
    startOfNextMonth_,

    -- *** Text
    like_,

    -- *** Industry
    ff12_,
    ff48_,
    highTechLr_,
    oneDigitSic_,
    twoDigitSic_,
    threeDigitSic_,
    fourDigitSic_,
    lastDigitSic_,

    -- *** Numeric converstion
    fromIntegral_,
    asInt_,
    asFloat_,

    -- * Conditional
    ifThenElse_,
    elseIf_,
    if_,
    (~>),
    (|>),
    (|~),
    firstNotNull_,
    isNull_,
    isNotNull_,

    -- * Sampling
    subsample_,
    union_,
    precomputedSubsample_,

    -- * General conversion
    coerce_,

    -- * Importing
    fromCsv_,
) where

import Data.Singletons
import Foghorn.Base
import Foghorn.Dsl.Algebra.Exorcism ()
import Foghorn.Dsl.Algebra.Var.Internal
import GHC.TypeLits
import Relude hiding ((&&), (/=), (<), (<=), (==), (>), (>=), (||), Comparison)

--------------------------------------------------------------------------------
-- Constants                                                                  --
--------------------------------------------------------------------------------

-- | Create an integer constant variable.
--
-- >>> int_ 42
-- Constant (CInt 42)
int_ :: Int -> EVar xsource i Int_
int_ = Constant . CInt

-- | Create a date constant variable from year, month, and day.
--
-- >>> date_ 2020 12 31
-- Constant (CDate 2020 12 31)
date_ :: Int -> Int -> Int -> EVar xsource i Date_
date_ y m d = Constant $ CDate y m d

-- | Create a text constant variable.
--
-- >>> text_ "Hello"
-- Constant (CText "Hello")
text_ :: Text -> EVar xsource i Text_
text_ = Constant . CText

-- | Create a floating-point constant variable.
--
-- >>> float_ 3.14
-- Constant (CFloat 3.14)
float_ :: Double -> EVar xsource i Float_
float_ = Constant . CFloat

---------------------------------------------------------------------------------------------------
-- Algebraic operations                                                                           --
----------------------------------------------------------------------------------------------------
-- | 'Num' instance for econometric variables.
--
-- Enables standard arithmetic operations on variables:
--
-- * @+@ - Addition
-- * @*@ - Multiplication
-- * @-@ - Subtraction
-- * @abs@ - Absolute value (also available as 'abs_')
-- * @signum@ - Sign of number (-1, 0, or 1)
-- * Integer literals (via @fromInteger@)
--
-- ==== __Example__
--
-- @
-- -- Arithmetic operations
-- totalAssets = currentAssets + fixedAssets
-- grossProfit = revenue - cogs
-- netIncome = revenue * profitMargin
--
-- -- Integer literals
-- adjustedValue = marketCap + 1000
-- @
instance forall a i xsource. (EconNum a, IsConst Int a, SingI i, SingI a) => Num (EVar xsource i a) where
    (+) a b = Algebra $ Add (exorcise a) (exorcise b)
    (*) a b = Algebra $ Multiply (exorcise a) (exorcise b)
    (-) a b = Algebra $ Subtract (exorcise a) (exorcise b)
    abs a = Unary Abs (exorcise a)
    signum a = Unary Signum (exorcise a)
    fromInteger = cvar @Int @a . fromIntegral

-- | 'Fractional' instance for floating-point variables.
--
-- Enables division and rational number literals for float variables:
--
-- * @/@ - Division
-- * Rational literals (via @fromRational@)
--
-- ==== __Example__
--
-- @
-- -- Division
-- roa = netIncome / totalAssets
-- pricePerShare = marketCap / sharesOutstanding
--
-- -- Rational literals
-- threshold = returns > 0.1
-- percentage = ratio * 100.0
-- @
instance (SingI i) => Fractional (EVar xsource i Float_) where
    fromRational = Constant . CFloat . fromRational
    (/) a b = Algebra $ Divide (exorcise a) (exorcise b)

{- | Negate a numeric variable.

Multiplies the variable by -1. Convenience function for numeric negation.

==== __Example__

@
-- Negate returns to get losses
losses = negate_ returns

-- Express debt as negative value
netDebt = assets + negate_ debt
@
-}
negate_ :: (EconNum a, IsConst Int a, SingI i, SingI a) => EVar xsource i a -> EVar xsource i a
negate_ = (*) (-1)

--------------------------------------------------------------------------------
-- Date functions                                                             --
--------------------------------------------------------------------------------
{- | Calculate the number of days between two dates.

Returns the difference in days between two date variables. The result is
positive when the first date is after the second date, negative when before.

Recommended to use in infix notation for readability.

==== __Example__

@
-- Days since fiscal year end
daysSinceFyend = dataDate `differenceInDays_` fyend

-- Days between announcement and effective dates
dealDuration = effectiveDate `differenceInDays_` announcementDate

-- Trading days elapsed in the month
daysIntoMonth = tradingDate `differenceInDays_` monthStart
@
-}
differenceInDays_
    :: ( EconDate d
       , EconDate d'
       , SingI i
       , SingI d
       , SingI d'
       , XVar xsource ~ xvar
       )
    => EVar xsource i d
    -> EVar xsource i d'
    -> EVar xsource i Int_
differenceInDays_ a b = DbFunc $ DifferenceInDays (exorcise a) (exorcise b)

----------------------------------------------------------------------------------------------------
-- Variable naming                                                                                --
----------------------------------------------------------------------------------------------------
{- | Store an intermediate variable with a name.

Assigns a name to a variable computation, which causes it to be materialized
and stored in the database. This may improve performance when the same complex
calculation is used multiple times, as it only needs to be computed once.

==== __Example__

@
-- Store complex industry adjustment
industryAdjustedRoa = named_ "industry_adjusted_roa" $
    roa - mean_ (where_ (same_ industryCode)) roa

-- Reuse the stored variable multiple times
highPerformers = industryAdjustedRoa > float_ 0.1
performanceRank = percentile_ (by_ industryCode) industryAdjustedRoa
@
-}
named_
    :: (SingI i, SingI d)
    => Text
    -> EVar xsource i d
    -> EVar xsource i d
named_ t k = Stored . Named $ NamedVar t (exorcise k) Nothing

{- | Store an intermediate variable with a name within a subsample.

Like 'named_' but only materializes the variable for observations within the
specified subsample. Useful for storing intermediate calculations that are only
needed for a subset of the data.

==== __Example__

@
-- Store market-adjusted return only for NYSE stocks
nyseAdjustedReturn = namedIn_ nyseStocks "nyse_adjusted_return" $
    returns - mean_ (where_ (same_ tradingDate)) returns

-- Store winsorized earnings only for profitable firms
profitableWinsorized = namedIn_ (subsample_ (earnings > 0)) "profitable_winsorized" $
    meanWinsorized_ (by_ industryCode) 0.01 0.99 earnings
@
-}
namedIn_
    :: ( SingI i, SingI d
       , (Exorcise (Subsample (EVar xsource i Bool_)) (Subsample xvar))
       , XVar xsource ~ xvar
       )
    => Subsample (EVar xsource i Bool_)
    -> Text
    -> EVar xsource i d
    -> EVar xsource i d
namedIn_ ss t k = Stored . Named $ NamedVar t (exorcise k) (Just $ exorcise ss)

----------------------------------------------------------------------------------------------------
-- Time series operations                                                                         --
----------------------------------------------------------------------------------------------------
{- | Lag a variable by N time periods.

Returns the value of the variable from N periods earlier. The lag operation
respects the time index structure of the data.

==== __Example__

@
-- Prior month's return
priorMonthReturn = lagN_ 1 monthlyReturns

-- Trading day from 5 trading days ago
lag5DayPrice = lagN_ 5 dailyPrice

-- Quarterly earnings from 4 quarters ago
lag4QuarterEarnings = lagN_ 4 quarterlyEarnings
@
-}
lagN_
    :: ( SingI i, SingI d
       )
    => Natural
    -> EVar xsource i d
    -> EVar xsource i d
lagN_ = lagNG_ Nothing

{- | Lag a variable by N periods within a subsample.

Like 'lagN_' but restricts the lag operation to observations within the
specified subsample. Observations outside the subsample are treated as missing
when computing lags.

==== __Example__

@
-- Prior return, only within valid trading days
priorReturn = lagNIn_ validTradingDays 1 returns

-- Lag 2 periods within fiscal year observations
laggedValue = lagNIn_ fiscalYearObs 2 earnings
@
-}
lagNIn_
    :: (SingI i, SingI d)
    => Subsample (EVar xsource i Bool_)
    -> Natural
    -> EVar xsource i d
    -> EVar xsource i d
lagNIn_ = lagNG_ . Just

-- | Internal function for lag operations. Use 'lagN_' or 'lagNIn_' instead.
lagNG_
    :: (SingI i, SingI d)
    => Maybe (Subsample (EVar xsource i Bool_))
    -> Natural
    -> EVar xsource i d
    -> EVar xsource i d
lagNG_ ss n k = Stored . Time $ TimeVar (Lag n) (exorcise k) (exorcise <$> ss)

{- | Lag a variable by specific time periods.

Convenience functions for common lag operations:

* 'lag_' and 'lag1_' - one period lag
* 'lag2_' - two period lag
* 'lag3_' - three period lag
* 'lag4_' - four period lag
* 'lag5_' - five period lag

==== __Example__

@
-- Prior trading day's return
priorDayReturn = lag1_ dailyReturns

-- Price from 2 months ago
lag2MonthPrice = lag2_ monthlyPrice

-- Earnings from 5 quarters ago
lag5QuarterEarnings = lag5_ quarterlyEarnings
@
-}
lag_
    , lag1_
    , lag2_
    , lag3_
    , lag4_
    , lag5_
        :: (SingI i, SingI d)
        => EVar xsource i d
        -> EVar xsource i d
lag_ = lagN_ 1
lag1_ = lagN_ 1
lag2_ = lagN_ 2
lag3_ = lagN_ 3
lag4_ = lagN_ 4
lag5_ = lagN_ 5

{- | Lead (forward) a variable by N time periods.

Returns the value of the variable from N periods in the future. The forward
operation respects the time index structure of the data.

==== __Example__

@
-- Next trading day's price
nextDayPrice = forwardN_ 1 dailyPrice

-- Return 3 months ahead
forward3MonthReturn = forwardN_ 3 monthlyReturns

-- Quarterly earnings 2 quarters ahead
forward2QuarterEarnings = forwardN_ 2 quarterlyEarnings
@
-}
forwardN_
    :: (SingI i, SingI d)
    => Natural
    -> EVar xsource i d
    -> EVar xsource i d
forwardN_ = forwardNG_ Nothing

{- | Lead a variable by N periods within a subsample.

Like 'forwardN_' but restricts the forward operation to observations within the
specified subsample. Observations outside the subsample are treated as missing
when computing leads.

==== __Example__

@
-- Next return, only within valid trading days
nextReturn = forwardNIn_ validTradingDays 1 returns

-- Lead 2 periods within fiscal year observations
leadValue = forwardNIn_ fiscalYearObs 2 earnings
@
-}
forwardNIn_
    :: (SingI i, SingI d)
    => Subsample (EVar xsource i Bool_)
    -> Natural
    -> EVar xsource i d
    -> EVar xsource i d
forwardNIn_ = forwardNG_ . Just

-- | Internal function for forward operations. Use 'forwardN_' or 'forwardNIn_' instead.
forwardNG_
    :: (SingI i, SingI d)
    => Maybe (Subsample (EVar xsource i Bool_))
    -> Natural
    -> EVar xsource i d
    -> EVar xsource i d
forwardNG_ ss n k = Stored . Time $ TimeVar (Forward n) (exorcise k) (exorcise <$> ss)

{- | Lead a variable by specific time periods.

Convenience functions for common forward (lead) operations:

* 'forward_' and 'forward1_' - one period lead
* 'forward2_' - two period lead
* 'forward3_' - three period lead
* 'forward4_' - four period lead
* 'forward5_' - five period lead

==== __Example__

@
-- Next trading day's return
nextDayReturn = forward1_ dailyReturns

-- Return 2 months ahead
forward2MonthReturn = forward2_ monthlyReturns

-- Earnings 5 quarters ahead
forward5QuarterEarnings = forward5_ quarterlyEarnings
@
-}
forward_
    , forward1_
    , forward2_
    , forward3_
    , forward4_
    , forward5_
        :: (SingI i, SingI d)
        => EVar xsource i d
        -> EVar xsource i d
forward_ = forwardN_ 1
forward1_ = forwardN_ 1
forward2_ = forwardN_ 2
forward3_ = forwardN_ 3
forward4_ = forwardN_ 4
forward5_ = forwardN_ 5

----------------------------------------------------------------------------------------------------
-- Unary operations                                                                               --
----------------------------------------------------------------------------------------------------
{- | Absolute value of a floating-point variable.

Returns the absolute value of the input, removing any negative sign.

==== __Example__

@
-- Absolute value of returns
absReturns = abs_ returns

-- Distance from mean
deviation = abs_ (returns - meanReturns)
@
-}
abs_ :: (SingI i) => EVar xsource i Float_ -> EVar xsource i Float_
abs_ = Unary Abs . exorcise

{- | Natural logarithm of a floating-point variable.

Computes the natural logarithm (base e) of the input. Null for
non-positive values.

==== __Example__

@
-- Log of market cap
logMarketCap = ln_ marketCap

-- Log returns
logReturns = ln_ (1 + returns)
@
-}
ln_ :: (SingI i) => EVar xsource i Float_ -> EVar xsource i Float_
ln_ = Unary Ln . exorcise

{- | Base-10 logarithm of a floating-point variable.

Computes the common logarithm (base 10) of the input. Null for
non-positive values.

==== __Example__

@
-- Log base 10 of market cap
log10MarketCap = log_ marketCap

-- Order of magnitude
orderOfMagnitude = log_ sales
@
-}
log_ :: (SingI i) => EVar xsource i Float_ -> EVar xsource i Float_
log_ = Unary Log . exorcise

{- | Square of a floating-point variable.

Computes the square of the input (x²).

==== __Example__

@
-- Squared returns for variance calculations
squaredReturns = square_ returns

-- Distance squared
distanceSquared = square_ (x - meanX)
@
-}
square_ :: (SingI i) => EVar xsource i Float_ -> EVar xsource i Float_
square_ = Unary Square . exorcise

{- | Set default value to zero for missing observations.

When a value is missing (NULL), use 0 instead. Convenient for variables where
missing should be interpreted as zero, such as R&D expenditure or optional fees.

==== __Example__

@
-- Treat missing R&D as zero
rdExpense = defaultToZero_ rd

-- Sum including zeros for missing
totalExpense = advertising + defaultToZero_ rd
@
-}
defaultToZero_ :: (EconNum d, SingI i, SingI d) => EVar xsource i d -> EVar xsource i d
defaultToZero_ = Unary ReplaceMissingWithZero . exorcise

{- | Set default value for missing observations.

When a value is missing (NULL), use the specified default instead.
More general than 'defaultToZero_'.

Recommended to use in infix notation for readability with simple expressions.
For complex expressions, consider using 'withDefault_' in prefix form.

==== __Example__

@
-- Defaults to industry average
adjustedLeverage = leverage `defaultTo_` industryAvgLeverage

-- Defaults to median
filledValue = originalValue `defaultTo_` medianValue

-- Defaults to zero
result = xint `defaultTo_` 0
@
-}
defaultTo_ :: (SingI i, SingI d) => EVar xsource i d -> EVar xsource i d -> EVar xsource i d
defaultTo_ a def = Unary (ReplaceMissing $ exorcise def) $ exorcise a

{- | Set default value for missing observations (default-first order).

Flipped version of 'defaultTo_' that takes the default value first, making
it more natural to use in prefix notation where the default value should be
prominent. Particularly useful for complex expressions where infix notation
becomes hard to read.

==== __Example__

@
-- Prefix form with default first (clear intent)
result = withDefault_ 0 complexCalculation

-- Good for complex boolean expressions
bothHighTech = withDefault_ false_ $
    highTechLr_ acqSic && highTechLr_ tgtSic

-- Multiple conditions
diversifying = withDefault_ false_ $
    crossIndustryDeal && differentCountries
@
-}
withDefault_ :: (SingI i, SingI d) => EVar xsource i d -> EVar xsource i d -> EVar xsource i d
withDefault_ = flip defaultTo_

---------------------------------------------------------------------------------------------------
-- Indicators                                                                                     --
----------------------------------------------------------------------------------------------------
{- | Convert a boolean variable to an indicator variable.

Converts a boolean expression to a numeric indicator that takes the value 1
when the condition is true and 0 when false.

==== __Example__

@
-- Indicator for firms with positive earnings
profitableIndicator = indicator_ (earnings > 0)

-- Indicator for large cap firms
largeCapIndicator = indicator_ (marketCap > float_ 1000)

-- Indicator for firms in manufacturing
manufacturingIndicator = indicator_ (ff12_ sic == int_ 3)
@
-}
indicator_
    :: (SingI i)
    => EVar xsource i Bool_
    -> EVar xsource i Indicator_
indicator_ = Indicator . exorcise

{- | Create an indicator that is 1 if the variable is positive, 0 otherwise.

Returns 1 when the variable value is strictly greater than zero, and 0 when
the value is zero or negative. Not defined for missing variables.

Can be combined with other functions for more complex behavior. For example,
to treat missing values as zero:

==== __Example__

@
-- Indicator for positive net income
positiveNi = indicatorPositive_ netIncome

-- Indicator for positive R&D, treating missing as zero
positiveRd = indicatorPositive_ (defaultToZero_ rd)

-- Using function composition
positiveRdAlt = indicatorPositive_ . defaultToZero_ $ rd
@
-}
indicatorPositive_
    :: forall i d xsource xvar
     . ( EconNum d
       , IsConst Int d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> EVar xsource i Indicator_
indicatorPositive_ v = indicator_ $ v > cvar @Int @d 0

----------------------------------------------------------------------------------------------------
-- Logical operations                                                                          --
----------------------------------------------------------------------------------------------------
{- | Logical AND operator.

Combines two boolean variables with logical conjunction. Returns true only when
both operands are true.

__Note:__ Conflicts with Prelude's '&&'. Use '&&_' or 'and_' to avoid conflicts,
or hide Prelude's version.

==== __Example__

@
-- Firms with positive earnings and high market cap
profitableAndLarge = (earnings > 0) && (marketCap > float_ 1000)

-- Same industry and same year
sameIndustryYear = (industryCode == otherIndustryCode) && (fyear == otherFyear)
@
-}
infixr 3 &&
(&&)
    :: (SingI i)
    => EVar xsource i Bool_
    -> EVar xsource i Bool_
    -> EVar xsource i Bool_
(&&) a b = Logic $ LogicAnd (exorcise a) (exorcise b)

{- | Logical AND operator (dot-suffixed version).

Identical to '&&' but with dot suffix to avoid conflicts with
Prelude's boolean AND. Recommended for use in modules that need both DSL
and regular boolean operations.

==== __Example__

@
-- No conflict with Prelude &&
profitableAndLarge = (earnings >' 0) &&' (marketCap >' float_ 1000)
@
-}
infixr 3 &&.
(&&.)
    :: (SingI i)
    => EVar xsource i Bool_
    -> EVar xsource i Bool_
    -> EVar xsource i Bool_
(&&.) = (&&)

{- | Logical AND operator (word version).

Identical to '&&' and '&&'' but using a word instead of symbols.

==== __Example__

@
-- Infix notation
profitableAndLarge = (earnings >' 0) `and_` (marketCap >' float_ 1000)
@
-}
and_
    :: (SingI i)
    => EVar xsource i Bool_
    -> EVar xsource i Bool_
    -> EVar xsource i Bool_
and_ = (&&)

{- | Logical OR operator.

Combines two boolean variables with logical disjunction. Returns true when at
least one operand is true.

__Note:__ Conflicts with Prelude's '||'. Use '||_' or 'or_' to avoid conflicts,
or hide Prelude's version.

==== __Example__

@
-- Firms in tech or healthcare industries
techOrHealthcare = (ff12_ sic == int_ 10) || (ff12_ sic == int_ 11)

-- High leverage or negative earnings
distressed = (leverage > float_ 0.8) || (earnings < 0)
@
-}
infixr 3 ||
(||)
    :: (SingI i)
    => EVar xsource i Bool_
    -> EVar xsource i Bool_
    -> EVar xsource i Bool_
(||) a b = Logic $ LogicOr (exorcise a) (exorcise b)

{- | Logical OR operator (dot-suffixed version).

Identical to '||' but with dot suffix to avoid conflicts with
Prelude's boolean OR. Recommended for use in modules that need both DSL
and regular boolean operations.

==== __Example__

@
-- No conflict with Prelude ||
techOrHealthcare = (ff12_ sic ==' int_ 10) ||' (ff12_ sic ==' int_ 11)
@
-}
infixr 3 ||.
(||.)
    :: (SingI i)
    => EVar xsource i Bool_
    -> EVar xsource i Bool_
    -> EVar xsource i Bool_
(||.) = (||)

{- | Logical OR operator (word version).

Identical to '||' and '||'' but using a word instead of symbols.

==== __Example__

@
-- Infix notation
techOrHealthcare = (ff12_ sic ==' int_ 10) `or_` (ff12_ sic ==' int_ 11)
@
-}
or_
    :: (SingI i)
    => EVar xsource i Bool_
    -> EVar xsource i Bool_
    -> EVar xsource i Bool_
or_ = (||)

{- | Greater than comparison operator.

Compares two variables and returns true when the left operand is strictly
greater than the right operand.

__Note:__ Conflicts with Prelude's '>'. Use '>.' to avoid conflicts,
or hide Prelude's version.

==== __Example__

@
-- Firms with market cap above 1 billion
largeCap = marketCap > float_ 1000

-- Returns above 10%
highReturns = returns > float_ 0.1
@
-}
infix 4 >
(>)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(>) a b = Comparison $ Greater (exorcise a) (exorcise b)

{- | Greater than comparison operator (dot-suffixed version).

Identical to '>' but with dot suffix to avoid conflicts with
Prelude's comparison operator. Recommended for use in modules that need both
DSL and regular comparisons.

==== __Example__

@
-- No conflict with Prelude >
largeCap = marketCap >_ float_ 1000
@
-}
infix 4 >.
(>.)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(>.) = (>)

{- | Greater than or equal comparison operator.

Compares two variables and returns true when the left operand is greater than
or equal to the right operand.

__Note:__ Conflicts with Prelude's '>='. Use '>=.' to avoid conflicts,
or hide Prelude's version.

==== __Example__

@
-- Firms with at least 100 employees
minEmployees = employees >= int_ 100

-- Non-negative returns
nonNegativeReturns = returns >= 0
@
-}
infix 4 >=
(>=)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(>=) a b = Comparison $ GreaterEqual (exorcise a) (exorcise b)

{- | Greater than or equal comparison operator (dot-suffixed version).

Identical to '>=' but with dot suffix to avoid conflicts with
Prelude's comparison operator.

==== __Example__

@
minEmployees = employees >=_ int_ 100
@
-}
infix 4 >=.
(>=.)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(>=.) = (>=)

{- | Equality comparison operator.

Compares two variables and returns true when they have equal values.

__Note:__ Conflicts with Prelude's '=='. Use '==.' to avoid conflicts,
or hide Prelude's version.

==== __Example__

@
-- Firms in the manufacturing industry (FF12 code 3)
manufacturing = ff12_ sic == int_ 3

-- Same fiscal year
sameFyear = fyear == otherFyear
@
-}
infix 4 ==
(==)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(==) a b = Comparison $ Equal (exorcise a) (exorcise b)

{- | Equality comparison operator (dot-suffixed version).

Identical to '==' but with dot suffix to avoid conflicts with
Prelude's equality operator.

==== __Example__

@
manufacturing = ff12_ sic ==_ int_ 3
@
-}
infix 4 ==.
(==.)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(==.) = (==)

{- | Less than or equal comparison operator.

Compares two variables and returns true when the left operand is less than
or equal to the right operand.

__Note:__ Conflicts with Prelude's @<=@ operator. Use '<=.' to avoid conflicts,
or hide the Prelude version.

==== __Example__

@
-- Small cap firms (market cap under 500M)
smallCap = marketCap <= float_ 500

-- Leverage below 50%
lowLeverage = leverage <= float_ 0.5
@
-}
infix 4 <=
(<=)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(<=) a b = Comparison $ LessEqual (exorcise a) (exorcise b)

{- | Less than or equal comparison operator (dot-suffixed version).

Identical to '<=' but with dot suffix to avoid conflicts with
Prelude's comparison operator.

==== __Example__

@
smallCap = marketCap <=_ float_ 500
@
-}
infix 4 <=.
(<=.)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(<=.) = (<=)

{- | Less than comparison operator.

Compares two variables and returns true when the left operand is strictly
less than the right operand.

__Note:__ Conflicts with Prelude's @<@ operator. Use '<.' to avoid conflicts,
or hide the Prelude version.

==== __Example__

@
-- Firms with negative earnings
negativeEarnings = earnings < 0

-- Dates before 2020
before2020 = fyear < int_ 2020
@
-}
infix 4 <
(<)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(<) a b = Comparison $ Less (exorcise a) (exorcise b)

{- | Less than comparison operator (dot-suffixed version).

Identical to '<' but with dot suffix to avoid conflicts with
Prelude's comparison operator.

==== __Example__

@
negativeEarnings = earnings <_ 0
@
-}
infix 4 <.
(<.)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(<.) = (<)

{- | Inequality comparison operator.

Compares two variables and returns true when they have different values.

__Note:__ Conflicts with Prelude's @/=@ operator. Use '/=.' or '!=.' to avoid
conflicts, or hide the Prelude version.

==== __Example__

@
-- Firms not in manufacturing
notManufacturing = ff12_ sic /= int_ 3

-- Different fiscal years
differentFyear = fyear /= otherFyear
@
-}
infix 4 /=
(/=)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(/=) a b = Comparison $ NotEqual (exorcise a) (exorcise b)

{- | Inequality comparison operator (dot-suffixed version).

Identical to '/=' but with dot suffix to avoid conflicts with
Prelude's inequality operator.

==== __Example__

@
notManufacturing = ff12_ sic /=_ int_ 3
@
-}
infix 4 /=.
(/=.)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(/=.) = (/=)

{- | Inequality comparison operator (alias for '/=').

Alternative syntax for inequality comparison. Identical to '/='.

==== __Example__

@
-- Firms not in manufacturing
notManufacturing = ff12_ sic != int_ 3
@
-}
infix 4 !=
(!=)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(!=) = (/=)

{- | Inequality comparison operator (dot-suffixed version, alias for '/=_').

Identical to '!=' and '/=_'. Alternative syntax for inequality comparison.

==== __Example__

@
notManufacturing = ff12_ sic !=_ int_ 3
@
-}
infix 4 !=.
(!=.)
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i Bool_
(!=.) = (/=)

{- | Membership test operator.

Tests whether a variable's value is in a list of values. Returns true when the
variable matches any element in the list.

==== __Example__

@
-- Firms in tech, healthcare, or consumer industries
selectedIndustries = ff12_ sic `in_` [int_ 10, int_ 11, int_ 4]

-- Fiscal years in analysis period
analysisYears = fyear `in_` [int_ 2015, int_ 2016, int_ 2017]
@
-}
in_
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> [EVar xsource i d]
    -> EVar xsource i Bool_
in_ a bs =
    Logic $ LogicIn (exorcise a) (exorcise <$> bs)

{- | Negative membership test operator.

Tests whether a variable's value is not in a list of values. Returns true when
the variable does not match any element in the list. Equivalent to @not_ . in_@.

==== __Example__

@
-- Firms not in financial or utility industries
nonFinancial = ff12_ sic `notIn_` [int_ 8, int_ 12]

-- Exclude specific fiscal years
excludeYears = fyear `notIn_` [int_ 2008, int_ 2009]
@
-}
notIn_
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> [EVar xsource i d]
    -> EVar xsource i Bool_
notIn_ a = not_ . in_ a

{- | Logical NOT operator.

Negates a boolean variable. Returns true when the operand is false, and false
when the operand is true.

==== __Example__

@
-- Firms without negative earnings
notNegative = not_ (earnings < 0)

-- Firms not in manufacturing
notManufacturing = not_ (ff12_ sic == int_ 3)
@
-}
not_
    :: (SingI i)
    => EVar xsource i Bool_
    -> EVar xsource i Bool_
not_ = Logic . LogicNot . exorcise

{- | Boolean constant representing true.

Creates a boolean variable that is always true.

==== __Example__

@
-- Always include all observations
allObservations = subsample_ true_

-- Default condition
defaultCondition = true_
@
-}
true_ :: EVar xsource i Bool_
true_ = Constant CTrue

{- | Boolean constant representing false.

Creates a boolean variable that is always false.

==== __Example__

@
-- Exclude all observations
noObservations = subsample_ false_

-- Placeholder for disabled conditions
disabledCondition = false_
@
-}
false_ :: EVar xsource i Bool_
false_ = Constant CFalse

----------------------------------------------------------------------------------------------------
-- Peer group operations                                                                          --
----------------------------------------------------------------------------------------------------
{- | Match observations with the same value for a variable.

Used with 'where_' to specify peer groups that share a common characteristic.
Creates a peer condition that matches when the peer's value equals the focal
observation's value.

==== __Example__

@
-- Mean of firms in the same industry
industryMean = mean_ (where_ (same_ industryCode)) marketCap

-- Mean of firms in the same industry and year
sameIndustryYearMean = mean_ (where_ (same_ industryCode, same_ fyear)) returns
@
-}
same_
    :: ( SingI i, SingI d
       )
    => EVar xsource i d
    -> Peer xsource i
same_ = Peer PSame . exorcise

{- | Match observations with different values for a variable.

Used with 'where_' to exclude observations that share a characteristic. Creates
a peer condition that matches when the peer's value differs from the focal
observation's value.

==== __Example__

@
-- Mean of firms excluding those with same CEO
excludeSameCeo = mean_ (where_ (different_ ceoId)) compensation

-- Count of firms in same industry but different state
crossStateCount = count_ (where_ (same_ industryCode, different_ state))
@
-}
different_
    :: ( SingI i, SingI d
       )
    => EVar xsource i d
    -> Peer xsource i
different_ = Peer PDifferent . exorcise

{- | Match observations with lesser values.

Used with 'where_' to specify ordering in peer groups. Creates a peer condition
that matches when the peer's value is strictly less than the focal observation's
value. Commonly used with dates for temporal precedence or with numeric values
for range filtering.

==== __Example__

@
-- Count of prior observations for the same firm
priorCount = count_ (where_ (same_ permno, less_ tradingDate)) returns

-- Mean return of earlier observations in same year
priorYearMean = mean_ (where_ (same_ fyear, less_ dataDate)) returns

-- Mean of firms with lower market cap
smallerFirmsMean = mean_ (where_ (less_ marketCap)) returns
@
-}
less_
    :: ( SingI i, SingI d
       )
    => EVar xsource i d
    -> Peer xsource i
less_ = Peer PLess . exorcise

{- | Match observations where a variable falls within a range.

Used with 'where_' to filter peer groups by numeric or date ranges. Creates a
peer condition that matches when the peer's value is between the lower and upper
bounds (inclusive).

==== __Example__

@
-- Mean of firms with similar market cap (within 10% of focal firm)
similarSizeMean = mean_ (where_ (between_ marketCap (marketCap * 0.9, marketCap * 1.1))) returns

-- Mean of firms with market cap between 100M and 1B
midCapMean = mean_ (where_ (between_ marketCap (float_ 100, float_ 1000))) returns
@
-}
between_
    :: ( SingI i, SingI d
       )
    => EVar xsource i d
    -> (EVar xsource i d, EVar xsource i d)
    -> Peer xsource i
between_ base (lower, upper) =
    Peer (PBetween (exorcise lower) (exorcise upper)) $ exorcise base

----------------------------------------------------------------------------------------------------
-- Functions                                                                                      --
----------------------------------------------------------------------------------------------------
-- | Extract the year component from a date.
--
-- ==== __Example__
--
-- @
-- year = extractYear_ announcementDate
-- @
extractYear_
    :: ( EconDate d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> EVar xsource i Int_
extractYear_ = DbFunc . ExtractYear . exorcise

-- | Extract the month component from a date (1-12).
--
-- ==== __Example__
--
-- @
-- month = extractMonth_ announcementDate
-- @
extractMonth_
    :: ( EconDate d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> EVar xsource i Int_
extractMonth_ = DbFunc . ExtractMonth . exorcise

-- | Extract the day component from a date (1-31).
--
-- ==== __Example__
--
-- @
-- day = extractDay_ announcementDate
-- @
extractDay_
    :: ( EconDate d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> EVar xsource i Int_
extractDay_ = DbFunc . ExtractDay . exorcise

-- | Add or subtract a time offset from a date.
--
-- Use with 'DayOffset', 'MonthOffset', 'QuarterOffset', or 'YearOffset' to
-- shift dates forward or backward in time.
--
-- ==== __Example__
--
-- @
-- -- Add 30 days
-- futureDate = offsetDate_ (DayOffset 30) startDate
--
-- -- Subtract 1 year
-- priorYear = offsetDate_ (YearOffset (-1)) currentDate
-- @
offsetDate_
    :: ( EconDate d
       , SingI i, SingI d
       )
    => OffsetDateType
    -> EVar xsource i d
    -> EVar xsource i d
offsetDate_ od = DbFunc . OffsetDate od . exorcise

-- | Get the first day of the month for a given date.
--
-- ==== __Example__
--
-- @
-- monthStart = startOfMonth_ tradingDate
-- @
startOfMonth_
    :: ( EconDate d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> EVar xsource i d
startOfMonth_ = DbFunc . StartOfMonth . exorcise

-- | Get the first day of the next month for a given date.
--
-- ==== __Example__
--
-- @
-- nextMonthStart = startOfNextMonth_ tradingDate
-- @
startOfNextMonth_
    :: ( EconDate d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> EVar xsource i d
startOfNextMonth_ =
    DbFunc
        . OffsetDate (MonthOffset 1)
        . exorcise
        . startOfMonth_

-- | Convert 4-digit SIC code to Fama-French 12 industry classification.
--
-- ==== __Example__
--
-- @
-- industry12 = ff12_ sicCode
-- @
ff12_
    :: ( SingI i )
    => EVar xsource i Sic4_
    -> EVar xsource i Ff12_
ff12_ = DbFunc . FamaFrench12 . exorcise

-- | Convert 4-digit SIC code to Fama-French 48 industry classification.
--
-- ==== __Example__
--
-- @
-- industry48 = ff48_ sicCode
-- @
ff48_
    :: ( SingI i
       )
    => EVar xsource i Sic4_
    -> EVar xsource i Ff48_
ff48_ = DbFunc . FamaFrench48 . exorcise

-- | Test if a 4-digit SIC code is high-tech per Loughran-Ritter classification.
--
-- ==== __Example__
--
-- @
-- isHighTech = highTechLr_ sicCode
-- @
highTechLr_
    :: ( SingI i )
    => EVar xsource i Sic4_
    -> EVar xsource i Bool_
highTechLr_ = DbFunc . HighTechLoughranRitter . exorcise

-- | Extract 1-digit SIC code from 4-digit SIC code.
--
-- ==== __Example__
--
-- @
-- sic1 = oneDigitSic_ sicCode
-- @
oneDigitSic_
    :: (SingI i)
    => EVar xsource i Sic4_
    -> EVar xsource i Sic1_
oneDigitSic_ = DbFunc . OneDigitSic . exorcise

-- | Extract 2-digit SIC code from 4-digit SIC code.
--
-- ==== __Example__
--
-- @
-- sic2 = twoDigitSic_ sicCode
-- @
twoDigitSic_
    :: (SingI i)
    => EVar xsource i Sic4_
    -> EVar xsource i Sic2_
twoDigitSic_ = DbFunc . TwoDigitSic . exorcise

-- | Extract 3-digit SIC code from 4-digit SIC code.
--
-- ==== __Example__
--
-- @
-- sic3 = threeDigitSic_ sicCode
-- @
threeDigitSic_
    :: (SingI i)
    => EVar xsource i Sic4_
    -> EVar xsource i Sic3_
threeDigitSic_ = DbFunc . ThreeDigitSic . exorcise

-- | Identity function for 4-digit SIC code (no conversion needed).
fourDigitSic_
    :: EVar xsource i Sic4_
    -> EVar xsource i Sic4_
fourDigitSic_ = id

-- | Extract the last digit from a 4-digit SIC code.
--
-- ==== __Example__
--
-- @
-- lastDigit = lastDigitSic_ sicCode
-- @
lastDigitSic_
    :: (SingI i)
    => EVar xsource i Sic4_
    -> EVar xsource i Int_
lastDigitSic_ sic = mod_ sic 10

-- | Convert an integral type to floating-point.
--
-- ==== __Example__
--
-- @
-- floatValue = fromIntegral_ intValue
-- @
fromIntegral_
    :: ( SingI i, SingI d
       , EconIntegral d
       )
    => EVar xsource i d
    -> EVar xsource i Float_
fromIntegral_ = DbFunc . FromIntegral . exorcise

-- | Convert an integral type to Int_.
--
-- ==== __Example__
--
-- @
-- intValue = asInt_ fyear
-- @
asInt_
    :: ( SingI i, SingI d
       , EconIntegral d
       )
    => EVar xsource i d
    -> EVar xsource i Int_
asInt_ = coerce_ -- DbFunc . AsInt . exorcise

-- | Convert an integral type to Float_.
--
-- Alias for 'fromIntegral_'.
asFloat_
    :: ( SingI i, SingI d
       , EconIntegral d
       )
    => EVar xsource i d
    -> EVar xsource i Float_
asFloat_ = fromIntegral_

-- | Coerce a variable from one econometric type to another.
--
-- Type-safe conversion between compatible econometric data types. The target
-- type must be specified via type application or type signature.
--
-- ==== __Example__
--
-- @
-- -- With type signature
-- fyearAsInt :: EVar xsource i Int_
-- fyearAsInt = coerce_ fyear
--
-- -- With type application
-- intVal = coerce_ @Int_ fyear
-- @
coerce_
    :: forall d' d xsource i t.
    ( SingI i, SingI d, SingI d'
    , EconCoerce d d'
    )
    => EVar xsource i d
    -> EVar xsource i d'
coerce_ = Coerce (demote @d') . exorcise

-- | Modulo operation for SIC codes.
--
-- Returns the remainder after division. Used internally by 'lastDigitSic_'.
mod_
    :: (SingI i)
    => EVar xsource i Sic4_
    -> Int
    -> EVar xsource i Int_
mod_ a f = DbFunc $ Mod (exorcise a) f

--------------------------------------------------------------------------------
-- Text functions                                                             --
--------------------------------------------------------------------------------

-- | SQL LIKE pattern matching for text variables.
--
-- Tests if a text variable matches a pattern with SQL wildcards. Use @%@ to
-- match any sequence of characters and @_@ to match any single character.
--
-- ==== __Example__
--
-- @
-- -- Match company names starting with "Tech"
-- isTechCompany = companyName \`like_\` "Tech%"
--
-- -- Match specific pattern
-- hasCorp = companyName \`like_\` "%Corp%"
-- @
like_
    :: ( SingI i
       )
    => EVar xsource i Text_
    -> Text
    -> EVar xsource i Bool_
like_ v = DbFunc . Like (exorcise v)

----------------------------------------------------------------------------------------------------
-- Aggregation                                                                                    --
----------------------------------------------------------------------------------------------------

-- | Create a peer condition from a single peer specification.
--
-- Constructs a 'JoinWhere' with a single peer condition. Use with peer operations
-- like 'same_', 'different_', 'less_', or 'between_' to define peer groups.
-- For multiple conditions, use 'where_' with tuple syntax.
--
-- ==== __Example__
--
-- @
-- -- Single peer condition
-- sameIndustry = where1_ (same_ industryCode)
-- @
where1_ :: (ToPeer xvar i a) => a -> JoinWhere xvar i
where1_ a = JoinWhere [toPeer a]

-- | Type class for converting tuples to peer conditions.
--
-- Allows using tuple syntax to specify multiple peer conditions concisely.
-- Use 'where_' directly with tuples for multiple conditions.
--
-- ==== __Example__
--
-- @
-- -- Multiple peer conditions with tuple syntax
-- sameIndustryYear = where_ (same_ industryCode, same_ fyear)
-- @
class ToJoinWhere xvar i a where
    where_ :: a -> JoinWhere xvar i

instance
    (ToPeer xvar i a, ToPeer xvar i b)
    => ToJoinWhere xvar i (a, b)
    where
    where_ (a, b) = JoinWhere [toPeer a, toPeer b]

instance
    (ToPeer xvar i a, ToJoinWhere xvar i (b, c))
    => ToJoinWhere xvar i (a, b, c)
    where
    where_ (a, b, c) = where1_ a <> where_ (b, c)

instance
    (ToPeer xvar i a, ToJoinWhere xvar i (b, c, d))
    => ToJoinWhere xvar i (a, b, c, d)
    where
    where_ (a, b, c, d) = where1_ a <> where_ (b, c, d)

instance
    (ToPeer xvar i a, ToJoinWhere xvar i (b, c, d, e))
    => ToJoinWhere xvar i (a, b, c, d, e)
    where
    where_ (a, b, c, d, e) = where1_ a <> where_ (b, c, d, e)

-- | Create a grouping specification from a single variable.
--
-- Constructs a 'By' with a single grouping variable. Use with aggregation
-- functions that support by-index grouping. For multiple grouping variables,
-- use 'by_' with tuple syntax.
--
-- ==== __Example__
--
-- @
-- -- Single grouping variable
-- byIndustry = by1_ industryCode
-- @
by1_
    :: ( Exorcise (EVar xsource i a) xvar
       , XVar xsource ~ xvar
       )
    => EVar xsource i a
    -> By xsource i
by1_ a = By [exorcise a]

-- | Type class for converting tuples to grouping specifications.
--
-- Allows using tuple syntax to specify multiple grouping variables concisely.
-- Use 'by_' directly with tuples for multiple grouping variables.
--
-- ==== __Example__
--
-- @
-- -- Multiple grouping variables with tuple syntax
-- byIndustryYear = by_ (industryCode, fyear)
-- @
class ToBy xvar i a where
    by_ :: a -> By xvar i

instance ( SingI i, SingI a, SingI b)
    => ToBy xsource i (EVar xsource i a, EVar xsource i b)
    where
    by_ (a, b) = By [exorcise a, exorcise b]

instance( SingI i, SingI a
    , ToBy xsource i (b, c)
    )
    => ToBy xsource i (EVar xsource i a, b, c)
    where
    by_ (a, b, c) = by1_ a <> by_ (b, c)

instance ( SingI i, SingI a
    , ToBy xsource i (b, c, d)
    )
    => ToBy xsource i (EVar xsource i a, b, c, d)
    where
    by_ (a, b, c, d) = by1_ a <> by_ (b, c, d)

instance( SingI i, SingI a
    , ToBy xsource i (b, c, d, e)
    )
    => ToBy xsource i (EVar xsource i a, b, c, d, e)
    where
    by_ (a, b, c, d, e) = by1_ a <> by_ (b, c, d, e)

--------------------------------------------------------------------------------
-- Sample aggregation                                                         --
--------------------------------------------------------------------------------

-- | General focal aggregation function (internal).
--
-- Base function for computing aggregations over peer groups. Most users should
-- use the specific aggregation functions like 'count_', 'mean_', 'max_', etc.
-- rather than calling this directly.
aggregateG
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Maybe (Subsample (EVar xsource i Bool_))
    -> AggregateComp (EVar xsource i d)
    -> JoinWhere xsource i
    -> Maybe (Having xsource i)
    -> EVar xsource i d
aggregateG ss comp jWhere having =
    Stored . Aggregate $ AggregateVar (exorcise comp) (AggregateFocal af)
  where
    af = AggFocal
        { joinWhere = exorcise jWhere
        , subsample = exorcise <$> ss
        , having = exorcise <$> having
        }

-- | Count observations in peer groups.
--
-- Returns the number of observations satisfying the peer conditions for each
-- observation. Uses focal aggregation, so each observation gets the count of
-- its peers.
--
-- ==== __Example__
--
-- @
-- -- Count firms in same industry and year
-- industryPeerCount = count_ (where1_ [same_ industryCode, same_ fyear])
-- @
count_
    :: ( SingI i )
    => JoinWhere xsource i
    -> EVar xsource i Int_
count_ jw = aggregateG Nothing AggCount jw Nothing

-- | Count observations in peer groups within a subsample.
--
-- Like 'count_' but restricts the aggregation to observations in the specified
-- subsample.
--
-- ==== __Example__
--
-- @
-- -- Count only large firms in same industry
-- largeFirmCount = countIn_ (subsample_ (assets > 1000))
--                           (where1_ [same_ industryCode, same_ fyear])
-- @
countIn_
    :: ( SingI i
       )
    => Subsample (EVar xsource i Bool_)
    -> JoinWhere xsource i
    -> EVar xsource i Int_
countIn_ ss jw = aggregateG (Just ss) AggCount jw Nothing

-- | Maximum value in peer groups.
--
-- Returns the maximum value among peers for each observation.
--
-- ==== __Example__
--
-- @
-- -- Largest firm in same industry
-- industryMaxSize = max_ firmSize (where1_ [same_ industryCode])
-- @
max_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
max_ kvar jw = aggregateG Nothing (AggMax kvar) jw Nothing

-- | Maximum value in peer groups within a subsample.
--
-- Like 'max_' but restricts to observations in the specified subsample.
maxIn_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Subsample (EVar xsource i Bool_)
    -> EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
maxIn_ ss kvar jw = aggregateG (Just ss) (AggMax kvar) jw Nothing

-- | Mean (average) value in peer groups.
--
-- Returns the arithmetic mean among peers for each observation.
--
-- ==== __Example__
--
-- @
-- -- Average deal size in same industry and year
-- industryMeanDealSize = mean_ dealValue (where1_ [same_ industryCode, same_ fyear])
-- @
mean_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
mean_ kvar jw = aggregateG Nothing (AggMean kvar) jw Nothing

-- | Mean value in peer groups within a subsample.
--
-- Like 'mean_' but restricts to observations in the specified subsample.
meanIn_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Subsample (EVar xsource i Bool_)
    -> EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
meanIn_ ss kvar jw = aggregateG (Just ss) (AggMean kvar) jw Nothing

-- | Winsorized mean in peer groups.
--
-- Returns the mean after winsorizing at the specified percentile level.
--
-- ==== __Example__
--
-- @
-- -- Mean winsorized at 1st and 99th percentiles
-- robustMean = meanWinsorized_ 0.01 returnValue (where1_ [same_ industryCode])
-- @
meanWinsorized_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Double
    -> EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
meanWinsorized_ p kvar jw = aggregateG Nothing (AggMeanWinsorized p kvar) jw Nothing

-- | Winsorized mean in peer groups within a subsample.
--
-- Like 'meanWinsorized_' but restricts to observations in the specified subsample.
meanWinsorizedIn_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Subsample (EVar xsource i Bool_)
    -> Double
    -> EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
meanWinsorizedIn_ ss p kvar jw = aggregateG (Just ss) (AggMeanWinsorized p kvar) jw Nothing

--------------------------------------------------------------------------------
-- Peer aggregation                                                           --
--------------------------------------------------------------------------------

-- | General peer aggregation function (internal).
--
-- Base function for computing aggregations over peer groups while automatically
-- excluding the focal observation itself. Adds a 'different_' condition to
-- ensure self-exclusion. Most users should use 'meanOfPeers_' or 'medianOfPeers_'
-- rather than calling this directly.
aggregatePeersG
    :: forall i d xsource xvar
     . ( EconNum d
       , SingI i, SingI d
       )
    => Maybe (Subsample (EVar xsource i Bool_))
    -> AggregateComp (EVar xsource i d)
    -> JoinWhere xsource i
    -> Maybe (Having xsource i)
    -> EVar xsource i d
aggregatePeersG ss method jWhere having =
    Stored . Aggregate $ AggregateVar (exorcise method) (AggregateFocal af)
  where
    xtype :: EVar xsource i Text_
    xtype = Constant . CText . show $ demote @i

    af =
        AggFocal
            { joinWhere = XJoinWhere [exorcise $ different_ xtype] <> exorcise jWhere
            , subsample = exorcise <$> ss
            , having = exorcise <$> having
            }

-- | Mean of peers, excluding the focal observation.
--
-- Computes the mean among peer observations, automatically excluding the
-- observation itself from its peer group calculation. This prevents
-- mechanical correlation between a firm's value and its peer group average.
--
-- ==== __Example__
--
-- @
-- -- Average Tobin's Q of other firms in same industry and year
-- peerTobinsQ = meanOfPeers_ tobinsQ (where1_ [same_ industryCode, same_ fyear])
--
-- -- Industry mean leverage excluding the firm itself
-- peerLeverage = meanOfPeers_ leverage (where1_ [same_ industryCode])
-- @
meanOfPeers_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
meanOfPeers_ evar jWhere =
    aggregatePeersG Nothing (AggMean evar) jWhere Nothing

-- | Mean of peers within a subsample, excluding the focal observation.
--
-- Like 'meanOfPeers_' but restricts to observations in the specified subsample.
meanOfPeersIn_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Subsample (EVar xsource i Bool_)
    -> EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
meanOfPeersIn_ ss evar jWhere =
    aggregatePeersG (Just ss) (AggMean evar) jWhere Nothing

-- | Median of peers, excluding the focal observation.
--
-- Computes the median among peer observations, automatically excluding the
-- observation itself from its peer group calculation.
--
-- ==== __Example__
--
-- @
-- -- Median Tobin's Q of other firms in same industry and year
-- peerMedianQ = medianOfPeers_ tobinsQ (where1_ [same_ industryCode, same_ fyear])
-- @
medianOfPeers_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
medianOfPeers_ evar jWhere =
    aggregatePeersG Nothing (AggMedian evar) jWhere Nothing

-- | Median of peers within a subsample, excluding the focal observation.
--
-- Like 'medianOfPeers_' but restricts to observations in the specified subsample.
medianOfPeersIn_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Subsample (EVar xsource i Bool_)
    -> EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
medianOfPeersIn_ ss evar jWhere = 
    aggregatePeersG (Just ss) (AggMedian evar) jWhere Nothing

-- | Compute a specific percentile within peer groups.
--
-- Returns the value at the specified percentile (0.0 to 1.0) among peers for
-- each observation. For example, 0.25 gives the 25th percentile (first quartile),
-- 0.5 gives the median, and 0.75 gives the 75th percentile (third quartile).
--
-- ==== __Example__
--
-- @
-- -- 75th percentile of firm size in same industry
-- sizeP75 = percentile_ 0.75 firmSize (where1_ [same_ industryCode])
--
-- -- 90th percentile of returns
-- returnP90 = percentile_ 0.90 returns (where1_ [same_ fyear])
-- @
percentile_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Double
    -> EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
percentile_ d kvar jw = aggregateG Nothing (AggPercentile d kvar) jw Nothing

-- | Compute a specific percentile within peer groups within a subsample.
--
-- Like 'percentile_' but restricts to observations in the specified subsample.
percentileIn_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Subsample (EVar xsource i Bool_)
    -> Double
    -> EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
percentileIn_ ss d kvar jw = aggregateG (Just ss) (AggPercentile d kvar) jw Nothing

-- | Sum of values in peer groups.
--
-- Computes the total (sum) among peers for each observation. The focal
-- observation is included in its own group total.
--
-- ==== __Example__
--
-- @
-- -- Total industry sales for each firm
-- industrySales = totalOfGroup_ sales (where1_ [same_ industryCode, same_ fyear])
-- @
totalOfGroup_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
totalOfGroup_ kvar jw = aggregateG Nothing (AggTotal kvar) jw Nothing

-- | Sum of values in peer groups within a subsample.
--
-- Like 'totalOfGroup_' but restricts to observations in the specified subsample.
totalOfGroupIn_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Subsample (EVar xsource i Bool_)
    -> EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
totalOfGroupIn_ ss kvar jw = aggregateG (Just ss) (AggTotal kvar) jw Nothing

-- | Herfindahl-Hirschman Index in peer groups.
--
-- Computes the Herfindahl index (sum of squared shares) among peers for each
-- observation. Measures market concentration where values closer to 1 indicate
-- higher concentration.
--
-- ==== __Example__
--
-- @
-- -- Industry concentration based on market shares
-- industryHHI = herfindahl_ marketShare (where1_ [same_ industryCode, same_ fyear])
-- @
herfindahl_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
herfindahl_ kvar jw = aggregateG Nothing (AggHerfindahl kvar) jw Nothing

-- | Herfindahl-Hirschman Index in peer groups within a subsample.
--
-- Like 'herfindahl_' but restricts to observations in the specified subsample.
herfindahlIn_
    :: ( EconNum d
       , SingI i, SingI d
       )
    => Subsample (EVar xsource i Bool_)
    -> EVar xsource i d
    -> JoinWhere xsource i
    -> EVar xsource i d
herfindahlIn_ ss kvar jw = aggregateG (Just ss) (AggHerfindahl kvar) jw Nothing

-- | Compound (cumulative) return in peer groups.
--
-- Computes the compound return by multiplying (1 + return) values across peers.
-- Used to calculate total returns over multiple periods or across a portfolio.
--
-- ==== __Example__
--
-- @
-- -- Cumulative return over a rolling window
-- cumulativeReturn = compoundReturn_ dailyReturn (where1_ [same_ permno,
--                                                           tradingDate `PBetween` (startDate, endDate)])
-- @
compoundReturn_
    :: ( SingI i
       )
    => EVar xsource i Float_
    -> JoinWhere xsource i
    -> EVar xsource i Float_
compoundReturn_ kvar jw = aggregateG Nothing (AggCompoundReturn kvar) jw Nothing

-- | Compound (cumulative) return in peer groups within a subsample.
--
-- Like 'compoundReturn_' but restricts to observations in the specified subsample.
compoundReturnIn_
    :: ( SingI i
       )
    => Subsample (EVar xsource i Bool_)
    -> EVar xsource i Float_
    -> JoinWhere xsource i
    -> EVar xsource i Float_
compoundReturnIn_ ss kvar jw = aggregateG (Just ss) (AggCompoundReturn kvar) jw Nothing

-- | Standard deviation in peer groups.
--
-- Computes the standard deviation among peers for each observation. Measures
-- the dispersion or volatility within the peer group.
--
-- ==== __Example__
--
-- @
-- -- Volatility of returns in same industry
-- industryVolatility = standardDeviation_ returns (where1_ [same_ industryCode, same_ fyear])
--
-- -- Rolling volatility over a window
-- rollingVol = standardDeviation_ returns (where1_ [same_ permno,
--                                                    tradingDate `PBetween` (startDate, endDate)])
-- @
standardDeviation_
    :: ( SingI i
       )
    => EVar xsource i Float_
    -> JoinWhere xsource i
    -> EVar xsource i Float_
standardDeviation_ kvar jw = aggregateG Nothing (AggStDev kvar) jw Nothing

-- | Standard deviation in peer groups within a subsample.
--
-- Like 'standardDeviation_' but restricts to observations in the specified subsample.
standardDeviationIn_
    :: ( SingI i )
    => Subsample (EVar xsource i Bool_)
    -> EVar xsource i Float_
    -> JoinWhere xsource i
    -> EVar xsource i Float_
standardDeviationIn_ ss kvar jw = aggregateG (Just ss) (AggStDev kvar) jw Nothing

--------------------------------------------------------------------------------
-- AggregationBy                                                              --
--------------------------------------------------------------------------------

-- | General by-index aggregation function (internal).
--
-- Base function for computing aggregations by explicit index grouping. Collapses
-- data to the specified index level, changing the index structure to match the
-- grouping variables. Most users should use 'totalOfGroupBy_' or 'herfindahlBy_'
-- rather than calling this directly.
aggregateByG
    :: ( EconNum a
       , SingI i, SingI i', SingI a, SingI x', SingI t' -- [i, i', t, t', a]
       , SetTimeSeries t' (SetCrossSection x' i) ~ i'
       )
    => AggregateComp (EVar xsource i a)
    -> (EVar xsource i x', EVar xsource i t')
    -> Maybe (JoinWhere xsource i)
    -> Maybe (Having xsource i)
    -> EVar xsource i' a
aggregateByG comp (xBy, tBy) jWhere having =
    Stored . Aggregate $ AggregateVar (exorcise comp) (AggregateBy ab)
  where
    ab =
        AggBy
            { byIndices = (Just $ exorcise xBy, Just $ exorcise tBy)
            , joinWhere = exorcise <$> jWhere
            , having = exorcise <$> having
            }

-- | Compute sum by explicit index grouping.
--
-- Calculates the total (sum) for each group defined by the specified
-- cross-sectional and time-series indices. Changes the index structure to
-- match the grouping variables.
--
-- ==== __Example__
--
-- @
-- -- Total deal value by industry and year
-- industryTotalDeals :: EVar xsource (Panel Industry_ Fyear_) Float_
-- industryTotalDeals = totalOfGroupBy_ dealValue
--                                      (industryCode, fyear)
--                                      Nothing
--
-- -- Total for publicly traded acquirers only
-- publicFirmDeals = totalOfGroupBy_ dealValue
--                                   (industryCode, fyear)
--                                   (Just $ where1_ [isPublic])
-- @
totalOfGroupBy_
    :: ( EconNum a
       , SingI i, SingI i', SingI x', SingI t', SingI a
       , SetTimeSeries t' (SetCrossSection x' i) ~ i'
       )
    => EVar xsource i a
    -> (EVar xsource i x', EVar xsource i t')
    -> Maybe (JoinWhere xsource i)
    -> EVar xsource i' a
totalOfGroupBy_ kvar bys jw = aggregateByG (AggTotal kvar) bys jw Nothing

-- | Compute Herfindahl index by explicit index grouping.
--
-- Calculates the Herfindahl-Hirschman Index (sum of squared shares) for each
-- group defined by the specified cross-sectional and time-series indices.
-- Changes the index structure to match the grouping variables.
--
-- ==== __Example__
--
-- @
-- -- Market concentration by industry and year
-- industryConcentration :: EVar xsource (Panel Industry_ Fyear_) Float_
-- industryConcentration = herfindahlBy_ marketShare
--                                       (industryCode, fyear)
--                                       Nothing
--
-- -- With peer condition to filter firms by market cap
-- concentrationOfLargeFirms = herfindahlBy_ marketShare
--                                           (industryCode, fyear)
--                                           (Just $ where1_ [marketCap `PBetween` (threshold1, threshold2)])
-- @
herfindahlBy_
    :: ( EconNum a
       , SingI i, SingI i', SingI x', SingI t', SingI a
       , SetTimeSeries t' (SetCrossSection x' i) ~ i'
       )
    => EVar xsource i a
    -> (EVar xsource i x', EVar xsource i t')
    -> Maybe (JoinWhere xsource i)
    -> EVar xsource i' a
herfindahlBy_ kvar bys jw = aggregateByG (AggHerfindahl kvar) bys jw Nothing

--------------------------------------------------------------------------------
-- Selecting functions                                                        --
--------------------------------------------------------------------------------

-- | Select only year-end observations from a panel dataset.
--
-- Filters the variable to include only observations at the end of each calendar
-- year, changing the time-series index to CalendarYear_. Commonly used to
-- convert monthly or daily data to annual frequency by selecting December end values.
--
-- ==== __Example__
--
-- @
-- -- Convert monthly stock returns to year-end values
-- yearEndReturn :: EVar xsource (Panel Permno_ CalendarYear_) Float_
-- yearEndReturn = yearEnd_ monthlyReturn
--
-- -- Use in aggregation to get yearly statistics
-- meanYearEndPrice = mean_ (yearEnd_ dailyPrice)
-- @
yearEnd_
    :: (SingI i, SingI i', SingI d,
        SetTimeSeries CalendarYear_ i ~ i')
    => EVar xsource i d
    -> EVar xsource i' a
yearEnd_ = Stored . Select . SelectVar YearEnd . exorcise

--------------------------------------------------------------------------------
-- Conditional evaluation                                                     --
--------------------------------------------------------------------------------

-- | Conditional expression (if-then-else).
--
-- Returns the first branch if the condition is true, otherwise the second branch.
-- Equivalent to SQL's CASE WHEN or ternary operator in other languages.
--
-- ==== __Example__
--
-- @
-- -- Classify deals as large or small
-- dealSize = ifThenElse_(dealValue > 1000) (text_ "large") (text_ "small")
--
-- -- Handle missing values
-- cleanedReturn = ifThenElse_(isNull_ rawReturn) (float_ 0) rawReturn
-- @
ifThenElse_
    :: (SingI i, SingI d)
    => EVar xsource i Bool_
    -> EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i d
ifThenElse_ condition branch1 branch2 =
    If (exorcise condition) (exorcise branch1) (exorcise branch2)

-- | Chained conditional expression (else-if).
--
-- Alias for 'ifThenElse_' to improve readability when chaining multiple conditions.
-- Use with 'ifThenElse_' to create nested conditionals.
--
-- ==== __Example__
--
-- @
-- -- Multi-way classification
-- dealType = ifThenElse_(dealValue > 10000)
--                (text_ "mega")
--                (elseIf_ (dealValue > 1000)
--                         (text_ "large")
--                         (text_ "small"))
-- @
elseIf_
    :: (SingI i, SingI d)
    => EVar xsource i Bool_
    -> EVar xsource i d
    -> EVar xsource i d
    -> EVar xsource i d
elseIf_ condition branch1 branch2 =
    If (exorcise condition) (exorcise branch1) (exorcise branch2)

{- | Infix conditional expression builder.

Provides a natural, readable syntax for multi-way conditionals using custom
operators. This is an alternative to nested 'ifThenElse_' expressions that reads more
like natural language.

The syntax uses three operators:

* '~>' - "then" operator, pairs a condition with its result
* '|>' - "else if" operator, chains additional conditions
* '|~' - "else" operator, provides the default case

__Important:__ All conditional expressions __must__ have an else term using '|~'.
If no action is needed for the else case, use 'null_' as the default value.

==== __Example__

@
-- Simple if-then-else
category = if_ $
    marketCap >. float_ 10000 ~> text_ "mega"
    |~ text_ "other"

-- Multi-way conditional
riskLevel = if_ $
    leverage >. float_ 0.8 ~> text_ "high"
    |> leverage >. float_ 0.5 ~> text_ "medium"
    |> leverage >. float_ 0.3 ~> text_ "low"
    |~ text_ "minimal"

-- With complex conditions
dealStatus = if_ $
    (dealValue >. float_ 1000) &&. completed ~> text_ "major_completed"
    |> (dealValue >. float_ 1000) ~> text_ "major_pending"
    |> completed ~> text_ "minor_completed"
    |~ text_ "minor_pending"

-- Using null_ when no else action needed
filtered = if_ $
    shouldInclude ~> originalValue
    |~ null_
@

See also: 'ifThenElse_', 'elseIf_' for the traditional function-based syntax.
-}
if_
    :: forall xsource i d. (SingI i, SingI d)
    => IfThenElse xsource i d
    -> EVar xsource i d
if_ ite = toDsl ifThens
    where
        IfThens ifThens = ite.iteIfThen

        toDsl :: [IfThen1 xsource i d] -> EVar xsource i d
        toDsl []  = ite.iteElse
        toDsl (ifThen1: ifs) = ifThenElse_ ifThen1.itIf ifThen1.itThen (toDsl ifs)

{- | Then operator for conditional expressions.

Pairs a boolean condition with its result value. Used as the first step in
building an 'if_' expression.

==== __Example__

@
-- Start a conditional with ~>
result = if_ $
    condition ~> thenValue
    |~ elseValue
@
-}
(~>)
    :: EVar xsource i Bool_
    -> EVar xsource i d
    -> IfThens xsource i d
(~>) a b = IfThens $ [IfThen1 a b]

infix 2 ~>

{- | Else-if operator for conditional expressions.

Chains additional condition-result pairs in a multi-way conditional.
Combines multiple 'IfThens' into a single expression.

==== __Example__

@
-- Chain multiple conditions with |>
classification = if_ $
    value >. float_ 100 ~> text_ "large"
    |> value >. float_ 50 ~> text_ "medium"
    |> value >. float_ 10 ~> text_ "small"
    |~ text_ "tiny"
@
-}
(|>)
    :: IfThens xsource i d
    -> IfThens xsource i d
    -> IfThens xsource i d
(|>) = (<>)

infix 1 |>

{- | Else operator for conditional expressions.

Provides the default value when no conditions match. This completes the
conditional expression and produces an 'IfThenElse' suitable for 'if_'.

__Required:__ Every 'if_' expression must end with '|~'. If no else action
is needed, use 'null_' as the default.

==== __Example__

@
-- Terminate with |~ for the else clause
rating = if_ $
    score >. float_ 90 ~> text_ "excellent"
    |> score >. float_ 70 ~> text_ "good"
    |~ text_ "needs improvement"

-- Use null_ when no else action needed
flagged = if_ $
    suspicious ~> text_ "FLAGGED"
    |~ null_
@
-}
(|~)
    :: IfThens xsource i d
    -> EVar xsource i d
    -> IfThenElse xsource i d
(|~) = IfThenElse

infix 1 |~

-- | Internal: Single condition-result pair for 'if_' expressions.
data IfThen1 xsource i d = IfThen1
    { itIf :: EVar xsource i Bool_
    -- ^ The condition to test
    , itThen :: EVar xsource i d
    -- ^ The result if condition is true
    }

-- | Internal: Chain of condition-result pairs for 'if_' expressions.
newtype IfThens xsource i d  = IfThens [IfThen1 xsource i d]
    deriving (Semigroup)

-- | Internal: Complete if-then-else expression with default case.
data IfThenElse xsource i d = IfThenElse
    { iteIfThen :: IfThens xsource i d
    -- ^ The chain of if-then conditions
    , iteElse :: EVar xsource i d
    -- ^ The default else value
    }

-- | Return the first non-null value from a list.
--
-- Evaluates variables in order and returns the first one that is not null.
-- Equivalent to SQL's COALESCE function.
--
-- ==== __Example__
--
-- @
-- -- Use primary value, fall back to secondary, then default
-- bestEstimate = firstNotNull_ [primaryEstimate, secondaryEstimate, float_ 0]
-- @
firstNotNull_
    :: (SingI i, SingI d)
    => [EVar xsource i d]
    -> EVar xsource i d
firstNotNull_ = foldr (\x -> ifThenElse_(isNotNull_ x) x) null_

-- | Test if a variable is null.
--
-- Returns a boolean variable that is true when the input is null.
--
-- ==== __Example__
--
-- @
-- hasMissingData = isNull_ returnValue
-- @
isNull_
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i Bool_
isNull_ = Null . exorcise

-- | Test if a variable is not null.
--
-- Returns a boolean variable that is true when the input is not null.
--
-- ==== __Example__
--
-- @
-- hasData = isNotNull_ returnValue
-- completeObservations = subsample_ (isNotNull_ returnValue)
-- @
isNotNull_
    :: (SingI i, SingI d)
    => EVar xsource i d
    -> EVar xsource i Bool_
isNotNull_ = NotNull . exorcise

-- | Null constant value.
--
-- Represents a missing value. Can be used with any data type.
--
-- ==== __Example__
--
-- @
-- -- Default to null if condition not met
-- conditionalValue = ifThenElse_condition someValue null_
-- @
null_ :: EVar xsource i d
null_ = Constant CNull

----------------------------------------------------------------------------------------------------
-- Reindexing                                                                                     --
----------------------------------------------------------------------------------------------------

-- | Constraint for valid reindexing transformations.
--
-- Ensures that the source index structure @i@ can be safely transformed to
-- the target index structure @i'@.
class Reindex i i'

-- | Change the index structure of a variable to a different index.
--
-- Transforms a variable from one index structure to another, such as converting
-- from firm-month to firm-year, or from deal-level to firm-level. Type
-- application is only required for inline transformations; when the target type
-- is specified in a type signature, it can be inferred.
--
-- ==== __Example__
--
-- @
-- -- Type signature approach (target type inferred)
-- yearlyReturn :: EVar xsource (Panel Permno_ Fyear_) Float_
-- yearlyReturn = reindex monthlyReturn
--
-- -- Inline with type application
-- meanOfYearly = mean_ (reindex @(Panel Permno_ Fyear_) monthlyReturn)
-- @
reindex
    :: forall i' i d xsource
     . ( Reindex i i'
       , SingI i, SingI i', SingI d
       )
    => EVar xsource i d
    -> EVar xsource i' d
reindex =
    Stored
        . Reindex
        . ReindexVar
            ReindexBasic
            (demote @i)
            (demote @i')
        . exorcise

-- | Change only the cross-sectional index of a variable.
--
-- Convenience function for reindexing that preserves the time-series index
-- while changing the cross-sectional dimension. 'reindexX' is most often used
-- inline with type application to make the intent explicit. While 'reindex'
-- can achieve the same transformation with type inference, 'reindexX' provides
-- type-level guarantees that only the cross-sectional index is modified.
-- 
-- ==== __Example__
--
-- @
-- -- Inline with type application (most common usage)
-- meanByFirm = mean_ (reindexX @Permno_ dealLevelVar)
-- @
reindexX
    :: forall cs' i' i d xsource
     . ( Reindex i i'
       , SetCrossSection cs' i ~ i'
       , SingI i
       , SingI i'
       , SingI d
       )
    => EVar xsource i d
    -> EVar xsource i' d
reindexX = reindex @i' @i @d

-- | Change only the time-series index of a variable.
--
-- Convenience function for reindexing that preserves the cross-sectional index
--while changing the time-series dimension. 'reindexT' is most often used inline
--with type application to make the intent explicit. While 'reindex' can achieve
--the same transformation with type inference, 'reindexT' provides type-level
--guarantees that only the time-series index is modified.
-- 
-- ==== __Example__
--
-- @
-- -- Inline with type application (most common usage)
-- meanYearly = mean_ (reindexT @Fyear_ monthlyData)
-- @
reindexT
    :: forall t' i' i d xsource xvar
     . ( Reindex i i'
       , SetTimeSeries t' i ~ i'
       , SingI i
       , SingI i'
       , SingI d
       )
    => EVar xsource i d
    -> EVar xsource i' d
reindexT = reindex @i' @i @d

--------------------------------------------------------------------------------
-- Sampling                                                                   --
--------------------------------------------------------------------------------

-- | Create a subsample filter from a boolean condition.
--
-- Restricts operations to observations where the condition is true. Commonly
-- used in aggregations, time series operations, and named variables.
--
-- ==== __Example__
--
-- @
-- -- Filter to large deals only
-- largeDealsSample = subsample_ (dealValue > 1000)
--
-- -- Use in aggregation
-- meanOfLargeDeals = meanIn_ dealValue \`by1_\` [industryCode] \`namedIn_\` largeDealsSample
-- @
subsample_ :: EVar xsource i Bool_ -> Subsample (EVar xsource i Bool_)
subsample_ = Subsample

-- | Combine two subsamples with logical OR.
--
-- Creates a subsample that includes observations satisfying either condition.
--
-- ==== __Example__
--
-- @
-- -- Include either large deals or stock-financed deals
-- combinedSample = subsample_ (dealValue > 1000) \`union_\` subsample_ (pctStock > 0.5)
-- @
union_
    :: Subsample (EVar xsource i Bool_)
    -> Subsample (EVar xsource i Bool_)
    -> Subsample (EVar xsource i Bool_)
union_ = Union

-- | Reference a precomputed subsample by name.
--
-- Allows reusing complex subsample definitions that were defined externally in
-- the database. 
--
-- ==== __Example__
--
-- @
-- -- Reference a precomputed sample stored in the database
-- mainSample = precomputedSubsample_ "main_analysis_sample"
-- @
precomputedSubsample_
    :: forall i xsource.
    (SingI i)
    => Text
    -> Subsample (EVar xsource i Bool_)
precomputedSubsample_ name =
    SubsamplePrecomputed name (demote @i)

----------------------------------------------------------------------------------------------------
-- Importing                                                                                      --
----------------------------------------------------------------------------------------------------

-- | Import a variable from a CSV file.
--
-- Creates a source variable that reads data from the specified CSV file path.
-- Type signature should specify the index structure and data type, or use type
-- application.
--
-- ==== __Example__
--
-- @
-- -- Type signature approach (preferred)
-- dealValue :: EVar XCoficatVar (Panel DealNumber_ AnnouncementDate_) Float_
-- dealValue = fromCsv_ "data/deals.csv"
--
-- -- Type application approach (creates variable with specified type)
-- dealValue = fromCsv_ @(Panel DealNumber_ AnnouncementDate_) @Float_ "data/deals.csv"
-- @
fromCsv_
    :: forall i d xsource.
    (SingI i, SingI d)
    => FilePath
    -> EVar xsource i d
fromCsv_ = Source . Source'Csv (demote @i) (demote @d)

--------------------------------------------------------------------------------
-- Undefined variables                                                        --
--------------------------------------------------------------------------------
-- | An undefined variable that allows one to easily specify the type of data
-- the variable will contain. For example, to mark a floating point variable
-- that will be defined later:
--
-- > var = undefined_ @Float_
--
-- Note that this isn't necessary with top-level type declarations. The above is
-- equivalent to:
--
-- > var :: EVar xsource i Float_
-- > var = undefined_
--
{-# WARNING undefined_ "undefined_ should only be used during development. Replace with actual variable definition before production use." #-}
undefined_ :: forall d x i. () => EVar x i d
undefined_ = error "undefined_: Replace placeholder with variable definition."
