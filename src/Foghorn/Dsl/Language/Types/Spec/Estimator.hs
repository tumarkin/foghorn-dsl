{- |
Module      : Foghorn.Dsl.Language.Types.Spec.Estimator
Description : Estimator type class and winsorization wrapper
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Defines the 'Estimator' type class that provides a uniform interface for
econometric estimation procedures (regressions, summary statistics, etc.).

Also provides the 'Winsorized' wrapper that adds automatic winsorization
to any estimator.

Most users will not import this module directly. Instead, import
"Foghorn.Dsl.Language.Types" which re-exports these types along with
core data structures and lens utilities.
-}

{-# LANGUAGE UndecidableInstances #-}

module Foghorn.Dsl.Language.Types.Spec.Estimator (
    -- * Estimator type class
    Estimator (..),

    -- * Winsorization
    Winsorized (..),
    winsorize_,
    winsorizeOnly_,
    winsorizeByType_,
    winsorizeFloats_,
) where

import Foghorn.Base
import Foghorn.Dsl.Algebra.Var.Internal (xEconType)
import Foghorn.Dsl.Language.Types.SpecVar (SpecVar (..))
import Foghorn.Dsl.Language.Types.Spec.Lenses
import Relude
import Relude.Extra.Lens (Lens', lens)

{- | Type class for econometric estimators.

Provides a uniform interface for different estimation procedures (regressions,
summary statistics, etc.). The type is parameterized by:

* @xsource@ - The variable source (e.g., 'XCoficatVar')
* @i@ - The index structure (e.g., 'Panel DealNumber_ AnnouncementDate_')
* @a@ - The concrete estimator type

==== __Methods__

* 'estimatorVariablesUsed' - Extract all variables used in the specification
* 'estimatorName' - Get a canonical name for the estimator

__Note:__ Code generation for a given target language is handled by
backend-specific type classes in the transpiler (see 'StataTranspilable' in
"Foghorn.Transpile.Stata.Class").


-}
class Estimator xsource i a | a -> xsource, a -> i where
    estimatorVariablesUsed :: a -> Set (SpecVar xsource i)
    estimatorName :: a -> Text

----------------------------------------------------------------------------------------------------
-- Winsorization                                                                                  --
----------------------------------------------------------------------------------------------------

{- | Wrapper that adds winsorization to an estimator.

Winsorization replaces extreme values (top and bottom percentiles) with less
extreme values to reduce the influence of outliers. 

==== __Fields__

* @thresholdFraction@ - Percentile threshold (e.g., 0.01 for 1%/99%)
* @winsorFilter@ - Optional predicate to select which variables to winsorize
* @estimator@ - The underlying estimator to wrap

==== __Example__

@
-- Winsorize all variables at 1%/99%
winsorized = winsorize_ 0.01 myRegression

-- Winsorize only float variables
winsorized = winsorizeFloats_ 0.01 myRegression

-- Winsorize only specific variables
winsorized = winsorizeOnly_ 0.01 (\\v -> v.name \`elem\` ["assets", "leverage"]) myRegression
@
-}
data Winsorized xsource i estimator
    = Winsorized
        { thresholdFraction :: Double
        , winsorFilter :: Maybe (SpecVar xsource i -> Bool)
        , estimator :: estimator
        }

-- | Lens for accessing the wrapped estimator
estimatorL :: Lens' (Winsorized xsource i estimator) estimator
estimatorL = lens (.estimator) (\s b -> s{estimator = b})

{- | Winsorize all variables in an estimator.

Applies winsorization to all variables at the specified threshold (as a
fraction, e.g., 0.01 for 1%/99% percentiles).
-}
winsorize_ :: Estimator xsource i estimator => Double -> estimator -> Winsorized xsource i estimator
winsorize_ d = Winsorized d Nothing

{- | Winsorize only variables matching a predicate.

Applies winsorization only to variables satisfying the filter function.
Other variables are left unchanged.
-}
winsorizeOnly_ :: Estimator xsource i estimator => Double -> (SpecVar xsource i -> Bool) -> estimator -> Winsorized xsource i estimator
winsorizeOnly_ d = Winsorized d . Just

{- | Winsorize variables based on their economic type.

Applies winsorization only to variables whose type matches the predicate.
Useful for winsorizing only numeric types while leaving indicators unchanged.
-}
winsorizeByType_ :: Estimator xsource i estimator => Double -> (EconType -> Bool) -> estimator -> Winsorized xsource i estimator
winsorizeByType_ d f = Winsorized d (Just typeF)
  where
    typeF :: SpecVar xsource i -> Bool
    typeF (SpecVar{xvar}) = f (xEconType xvar)

{- | Winsorize only float variables.

Common pattern that winsorizes continuous variables while leaving indicators,
dates, and identifiers unchanged.
-}
winsorizeFloats_ :: Estimator xsource i estimator => Double -> estimator -> Winsorized xsource i estimator
winsorizeFloats_ d = winsorizeByType_ d isEconFloat_
  where
    isEconFloat_ Float_ = True
    isEconFloat_ _ = False

----------------------------------------------------------------------------------------------------
-- Winsorized instances                                                                           --
----------------------------------------------------------------------------------------------------

instance (Estimator xsource i estimator, HasRhs xsource i estimator, HasLhs xsource i estimator, HasName estimator) => Estimator xsource i (Winsorized xsource i estimator) where
    estimatorVariablesUsed w = estimatorVariablesUsed w.estimator
    estimatorName w = estimatorName w.estimator <> "_w"

instance HasName estimator => HasName (Winsorized xsource i estimator) where
    nameL = estimatorL . nameL

instance HasLhs xsource i estimator => HasLhs xsource i (Winsorized xsource i estimator) where
    lhsL = estimatorL . lhsL

instance HasRhs xsource i estimator => HasRhs xsource i (Winsorized xsource i estimator) where
    rhsL = estimatorL . rhsL

instance HasConditions xsource i estimator => HasConditions xsource i (Winsorized xsource i estimator) where
    conditionsL = estimatorL . conditionsL

instance HasVce xsource i estimator => HasVce xsource i (Winsorized xsource i estimator) where
    vceL = estimatorL . vceL
