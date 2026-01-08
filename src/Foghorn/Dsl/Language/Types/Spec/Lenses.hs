{- |
Module      : Foghorn.Dsl.Language.Types.Spec.Lenses
Description : Lens type classes for manipulating estimator specifications
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Provides lens type classes and convenience functions for manipulating
estimator specifications in a generic way. These lenses allow you to modify
components (LHS, RHS, conditions, etc.) of any estimator without knowing
the concrete estimator type.

The module includes:

* __Lens type classes__ - 'HasName', 'HasLhs', 'HasRhs', 'HasConditions', 'HasVce', 'HasAbsorb'
* __Setter functions__ - Convenient ways to set values (e.g., 'name_', 'conditions_')
* __Transformation functions__ - Generic variable transformations (e.g., 'mapMaybeLhsVar')

Most users will not import this module directly. Instead, import
"Foghorn.Dsl.Language.Types" which re-exports these utilities along with
core data structures and the estimator abstraction.
-}

module Foghorn.Dsl.Language.Types.Spec.Lenses (
    -- * Name lens
    HasName (..),
    name_,

    -- * LHS lens
    HasLhs (..),
    lhs_,
    mapMaybeLhsVar,

    -- * RHS lens
    HasRhs (..),
    rhs_,
    addToRhs_,
    mapMaybeRhsVar,

    -- * Conditions lens
    HasConditions (..),
    conditions_,
    andCondition_,
    replaceCondition_,

    -- * VCE lens
    HasVce (..),
    vce_,

    -- * Absorb lens
    HasAbsorb (..),
    absorb_,
    mapMaybeAbsorbVar,
) where

import qualified Data.Set as Set
import Foghorn.Dsl.Language.Types.SpecVar (SpecVar)
import Foghorn.Dsl.Language.Types.Spec.Core
import Lens.Micro
import Relude

----------------------------------------------------------------------------------------------------
-- Name                                                                                           --
----------------------------------------------------------------------------------------------------

-- | Lens type class for estimator names.
class HasName spec where
    nameL :: Lens' spec Text

{- | Set the name of an estimator.

This setter function modifies an existing specification. The @&@ operator is
useful for chaining multiple modifications.

==== __Example__

@
-- Standard usage
spec' = name_ "baseline_regression" spec

-- Chaining with & operator
spec' = spec
    & name_ "baseline_regression"
    & vce_ RobustErrors
@
-}
name_ :: (HasName spec) => Text -> spec -> spec
name_ = set nameL

----------------------------------------------------------------------------------------------------
-- LHS                                                                                            --
----------------------------------------------------------------------------------------------------

-- | Lens type class for left-hand side (dependent) variable.
class HasLhs xsource i spec | spec -> xsource, spec -> i where
    lhsL :: Lens' spec (SpecVar xsource i)

{- | Transform the left-hand side variable with possible failure.

Applies a transformation that may fail (returning 'Nothing'). If the
transformation fails, the entire operation returns 'Nothing'.

Useful for conditional transformations or validation.
-}
mapMaybeLhsVar
    :: forall xsource i spec
     . (Eq xsource, HasLhs xsource i spec)
    => (SpecVar xsource i -> Maybe (SpecVar xsource i))
    -> spec
    -> Maybe spec
mapMaybeLhsVar f = lhsL f

{- | Set the left-hand side (dependent) variable of a specification.

This setter function modifies an existing specification. The @&@ operator is
useful for chaining multiple modifications.

==== __Example__

@
-- Standard usage
spec' = lhs_ [lhsQ| car |] spec

-- Chaining with & operator
spec' = spec
    & lhs_ [lhsQ| car |]
    & vce_ RobustErrors
@
-}
lhs_
    :: (HasLhs xsource i spec)
    => SpecVar xsource i
    -> spec
    -> spec
lhs_ = set lhsL
    

----------------------------------------------------------------------------------------------------
-- RHS                                                                                            --
----------------------------------------------------------------------------------------------------

-- | Lens type class for right-hand side (independent) variables.
class HasRhs xsource i spec | spec -> xsource, spec -> i where
    rhsL :: Lens' spec (Rhs (SpecVar xsource i))


{- | Set the right-hand side (independent) variables of a specification.

This setter function modifies an existing specification. The @&@ operator is
useful for chaining multiple modifications.

==== __Example__

@
-- Standard usage
spec' = rhs_ [rhsQ| dealValue + leverage |] spec

-- Chaining with & operator
spec' = spec
    & rhs_ [rhsQ| dealValue + leverage |]
    & vce_ RobustErrors
@
-}
rhs_
    :: (HasRhs xsource i spec)
    => Rhs (SpecVar xsource i)
    -> spec
    -> spec
rhs_ = set rhsL

{- | Transform right-hand side variables with possible failure.

Applies a transformation that may fail to each independent variable. If any
transformation fails, the entire operation returns 'Nothing'.
-}
mapMaybeRhsVar
    :: forall xsource i spec
     . (Eq xsource, HasRhs xsource i spec)
    => (SpecVar xsource i -> Maybe (SpecVar xsource i))
    -> spec
    -> Maybe spec
mapMaybeRhsVar f = rhsL (mapM f)

{- | Add variables to the right-hand side of a specification.

Appends additional independent variables to the existing RHS using '<>'.

This function modifies an existing specification. The @&@ operator is
useful for chaining multiple modifications.

==== __Example__

@
-- Standard usage
spec' = addToRhs_ [rhsQ| sizeControl + industryControl |] spec

-- Chaining with & operator
spec' = spec
    & addToRhs_ [rhsQ| sizeControl |]
    & addToRhs_ [rhsQ| industryControl |]
    & vce_ RobustErrors
@
-}
addToRhs_ :: (HasRhs xsource i spec) => Rhs (SpecVar xsource i) -> spec -> spec
addToRhs_ r = over rhsL (r <>)

----------------------------------------------------------------------------------------------------
-- Conditions                                                                                     --
----------------------------------------------------------------------------------------------------

-- | Lens type class for sample conditions.
class HasConditions xsource i spec | spec -> xsource, spec -> i where
    conditionsL :: Lens' spec (Maybe (Conditions (SpecVar xsource i)))

{- | Set the conditions for a specification.

Replaces any existing conditions with the provided ones.

This function modifies an existing specification. The @&@ operator is
useful for chaining multiple modifications.

==== __Example__

@
-- Standard usage
spec' = conditions_ [conditionsQ| publicTarget && completedDeal |] spec

-- Chaining with & operator
spec' = spec
    & conditions_ [conditionsQ| publicTarget |]
    & vce_ RobustErrors
@
-}
conditions_
    :: ( HasConditions xsource i spec
       , SpecVar xsource i ~ evar
       )
    => Maybe (Conditions evar)
    -> spec
    -> spec
conditions_ = set conditionsL

{- | Add a condition to a specification with AND logic.

If there are existing conditions, combines them with 'ConditionAnd'.
Otherwise, sets the condition as the only restriction.

This function modifies an existing specification. The @&@ operator is
useful for chaining multiple modifications.

==== __Example__

@
-- Standard usage
spec' = andCondition_ (Condition publicTarget) spec

-- Chaining with & operator
spec' = spec
    & andCondition_ (Condition publicTarget)
    & andCondition_ (Condition completedDeal)
    & vce_ RobustErrors
@
-}
andCondition_
    :: ( HasConditions xsource i spec
       , SpecVar xsource i ~ evar
       )
    => Conditions evar
    -> spec
    -> spec
andCondition_ c a =
    case  a^.conditionsL of
        Nothing -> set conditionsL (Just c) a
        Just cs -> set conditionsL (Just $ cs `ConditionAnd` c) a

{- | Replace specific conditions in a specification.

Traverses the condition tree and replaces any condition matching the test
predicate with the target condition.
-}
replaceCondition_
    :: forall xsource i spec evar.
       ( HasConditions xsource i spec
       , SpecVar xsource i ~ evar
       , Eq xsource
       )
    => (evar -> Bool)
    -- ^ A test to determine if the condition should be replaced
    -> evar
    -- ^ The replacement condition
    -> spec
    -> spec
replaceCondition_ test targetC spec =
    over (conditionsL . _Just) (fmap doReplace) spec
  where
    doReplace :: evar -> evar
    doReplace c = if test c then targetC else c

----------------------------------------------------------------------------------------------------
-- VCE                                                                                            --
----------------------------------------------------------------------------------------------------

-- | Lens type class for variance-covariance estimation (standard error calculation).
--
-- VCE specifies how standard errors should be computed: robust
-- (heteroskedasticity-consistent), clustered (by one or more variables), or
-- unadjusted (classical OLS).
class HasVce xsource i spec | spec -> xsource, spec -> i where
    vceL :: Lens' spec (Vce (SpecVar xsource i))

{- | Set the variance-covariance estimation method for a specification.

Specifies how standard errors should be computed in the regression.

This function modifies an existing specification. The @&@ operator is
useful for chaining multiple modifications.

==== __Example__

@
-- Standard usage with robust errors
spec' = vce_ RobustErrors spec

-- Use clustered standard errors
spec' = vce_ [clusterQ| firmId |] spec

-- Chaining with & operator
spec' = spec
    & lhs_ [lhsQ| car |]
    & rhs_ [rhsQ| dealValue + leverage |]
    & vce_ [clusterQ| firmId |]
@
-}
vce_
    :: ( HasVce xsource i spec
       , SpecVar xsource i ~ evar
       )
    => Vce evar
    -> spec
    -> spec
vce_ = set vceL

----------------------------------------------------------------------------------------------------
-- Absorb                                                                                         --
----------------------------------------------------------------------------------------------------

-- | Lens type class for absorbed fixed effects.
class HasAbsorb xsource i spec | spec -> xsource, spec -> i where
    absorbL :: Lens' spec (Set (SpecVar xsource i))

{- | Set the absorbed fixed effects for a specification.

Replaces any existing absorbed fixed effects with the provided ones.

This function modifies an existing specification. The @&@ operator is
useful for chaining multiple modifications.

==== __Example__

@
-- Standard usage
spec' = absorb_ [absorbQ| industry year |] spec

-- Chaining with & operator
spec' = spec
    & lhs_ [lhsQ| car |]
    & rhs_ [rhsQ| dealValue + leverage |]
    & absorb_ [absorbQ| industry year |]
    & vce_ [clusterQ| firmId |]
@
-}
absorb_ :: (HasAbsorb xsource i spec) => Set (SpecVar xsource i) -> spec -> spec
absorb_ = set absorbL

{- | Transform absorbed fixed effects with possible failure.

Applies a transformation that may fail to each absorbed variable. If any
transformation fails, the entire operation returns 'Nothing'.

Useful for filtering fixed effects or conditional transformations.
-}
mapMaybeAbsorbVar
    :: forall xsource i spec
     . (Eq xsource, Ord xsource, HasAbsorb xsource i spec)
    => (SpecVar xsource i -> Maybe (SpecVar xsource i))
    -> spec
    -> Maybe spec
mapMaybeAbsorbVar f = absorbL (fmap Set.fromList . mapM f . Set.toList)
