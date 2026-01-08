{- |
Module      : Foghorn.Dsl.Language.Types.Spec.Core
Description : Core data types for econometric specifications
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Defines the fundamental data types used in econometric specifications:

* 'Rhs' - Right-hand side variables (including interactions)
* 'Conditions' - Sample filtering conditions
* 'Vce' - Variance-covariance estimation methods
* 'Options' - Additional estimator-specific options

These types are combined by estimators (like 'Reg' and 'RegHdFe') to form
complete econometric specifications.

Most users will not import this module directly. Instead, import
"Foghorn.Dsl.Language.Types" which re-exports these types along with
the estimator abstraction and lens utilities.
-}

module Foghorn.Dsl.Language.Types.Spec.Core (
    -- * Right-hand side
    Rhs (..),
    getRhsVars,

    -- * Conditions
    Conditions (..),
    getConditionVars,

    -- * Variance-covariance
    Vce (..),
    getVceVars,

    -- * Options
    Options (..),
) where

import qualified Data.Set as Set
import Relude

----------------------------------------------------------------------------------------------------
-- Right-hand side                                                                                --
----------------------------------------------------------------------------------------------------

{- | Right-hand side variables in a specification.

Represents the independent variables in a regression or other specification.
Supports:

* Individual variables ('Var')
* Lists of variables ('RhsCons', also via '<>' operator)
* Interaction terms ('Interact' - without individual vars, 'FactorInteract' - with individual vars)

The 'Semigroup' instance allows building lists with '<>'.

__Note:__ Direct construction is typically not necessary. Use the 'rhsQ' quasi-quoter
from "Foghorn.Dsl.Language.Quote" which provides more convenient syntax:

@
[rhsQ| dealValue + leverage ## size |]
@

==== __Example__ (Direct Construction)

@
-- Single variable
simpleRhs = Var assets

-- Multiple variables
multipleRhs = Var assets <> Var leverage <> Var profitability

-- Interaction (without individual vars)
interaction = Interact (Var industry) (Var year)

-- Factor interaction (includes individual vars)
factorInteraction = FactorInteract (Var acquirerSize) (Var targetSize)
@
-}
data Rhs a
    = -- | A single RHS variable
      Var a
    | -- | A cons list of RHS variables
      RhsCons (Rhs a) (Rhs a)
    | -- | A RHS interaction term (not including individual vars)
      Interact (Rhs a) (Rhs a)
    | -- | A RHS interaction term (including individual vars)
      FactorInteract (Rhs a) (Rhs a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup (Rhs a) where
    a <> b = RhsCons a b

{- | Extract all variables from right-hand side specification.

Recursively traverses the RHS tree to collect all variables, including those
in interaction terms.
-}
getRhsVars :: (Ord a) => Rhs a -> Set a
getRhsVars = foldr Set.insert Set.empty

----------------------------------------------------------------------------------------------------
-- Conditions                                                                                     --
----------------------------------------------------------------------------------------------------

{- | Filtering conditions for specifications.

Represents restrictions on the sample used in estimation (e.g., Stata's @if@
clause). Conditions can be combined with logical operators.

__Note:__ Direct construction is typically not necessary. Use the 'conditionsQ'
or 'andConditionsQ' quasi-quoters from "Foghorn.Dsl.Language.Quote" which
provide more convenient syntax:

@
[conditionsQ| publicTarget && completedDeal |]
@

==== __Example__ (Direct Construction)

@
-- Single condition
publicDeals = Condition (specVar "public" isPublic)

-- Combined with AND
publicAndLarge = ConditionAnd publicDeals largeDeals

-- Combined with OR
publicOrPrivate = ConditionOr publicDeals privateDeals
@
-}
data Conditions a
    = -- | A single condition
      Condition a
    | ConditionAnd (Conditions a) (Conditions a)
    | ConditionOr (Conditions a) (Conditions a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | Extract all variables from conditions.

Recursively traverses the condition tree to collect all variables used in
filtering expressions.
-}
getConditionVars :: (Ord a) => Conditions a -> Set a
getConditionVars = foldr Set.insert Set.empty

----------------------------------------------------------------------------------------------------
-- Variance-covariance                                                                            --
----------------------------------------------------------------------------------------------------

{- | Variance-covariance estimation method.

Specifies how standard errors should be computed:

* 'RobustErrors' - Heteroskedasticity-robust (White) standard errors
* 'ClusteredErrors' - Clustered standard errors (specify clustering variables)
* 'UnadjustedErrors' - Classical OLS standard errors

__Note:__ For clustered errors, prefer using the 'clusterQ' quasi-quoter from
"Foghorn.Dsl.Language.Quote":

@
[clusterQ| dealNumber firm |]
@

Robust and unadjusted errors are constructed directly as shown above.
-}
data Vce a
    = RobustErrors
    | ClusteredErrors (Set a)
    | UnadjustedErrors
    deriving (Show, Eq, Foldable) 

{- | Extract clustering variables from VCE specification.

Returns the set of variables used for clustering (empty for robust or
unadjusted standard errors).
-}
getVceVars :: Vce a -> Set a
getVceVars RobustErrors = Set.empty
getVceVars (ClusteredErrors s) = s
getVceVars UnadjustedErrors = Set.empty

----------------------------------------------------------------------------------------------------
-- Options                                                                                        --
----------------------------------------------------------------------------------------------------

{- | Additional estimator-specific options.

Wrapper for arbitrary text options that are passed through to the underlying
estimator (e.g., Options not otherwise covered by the type system).
-}
newtype Options = Options {getOptions :: Text}
    deriving (Show, Eq)
