{- |
Module      : Foghorn.Dsl.Estimator.Reg
Description : Basic OLS regression estimator
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Provides the 'Reg' type for standard OLS regressions. This is a simpler
alternative to 'RegHdFe' for regressions without absorbed fixed effects.

The estimator supports:

* Left-hand side (dependent) variable
* Right-hand side (independent) variables with interactions
* Sample conditions (if clauses)
* Variance-covariance specifications (robust, clustered, or unadjusted errors)
* Additional options

= Usage

Typically constructed using the smart constructor and lens operations:

@
myReg :: Reg XCoficatVar (Panel DealNumber_ AnnouncementDate_)
myReg = reg_ "baseline" [lhsQ| car |] [rhsQ| dealValue + leverage |]
    & vce_ (ClusteredErrors [clusterQ| dealNumber |])
    & conditions_ [conditionsQ| publicTarget |]
@
-}

module Foghorn.Dsl.Estimator.Reg (
    -- * Estimators
    Reg (..),

    -- * Smart constructors
    reg_,
) where

import qualified Data.Set as Set
import Foghorn.Dsl.Language.Types
import Relude
import Relude.Extra.Lens (lens)

{- | Basic OLS regression specification.

Represents a standard linear regression without absorbed fixed effects.
For high-dimensional fixed effects, use 'RegHdFe' instead.

==== __Fields__

* @lhs@ - Dependent variable
* @rhs@ - Independent variables (may include interactions)
* @conditions@ - Optional sample restrictions (if clauses)
* @options@ - Additional options
* @vce@ - Variance-covariance estimation method
* @name@ - Model name for storage and output tables

==== __Instances__

* 'Estimator' - Full estimator interface (variables used, name extraction)
* 'HasName' - Access/modify the model name
* 'HasLhs' - Access/modify left-hand side variable
* 'HasRhs' - Access/modify right-hand side variables
* 'HasConditions' - Access/modify sample conditions
* 'HasVce' - Access/modify variance-covariance specification
-}
data Reg xsource i = Reg
    { lhs :: !(SpecVar xsource i)
    -- ^ Left-hand-side dependent variable
    , rhs :: !(Rhs (SpecVar xsource i))
    -- ^ Right-hand-side controls
    , conditions :: !(Maybe (Conditions (SpecVar xsource i)))
    -- ^ Conditions
    , options :: !(Maybe Options)
    -- ^ Set of fixed effect variables
    , vce :: !(Vce (SpecVar xsource i))
    -- ^ Clustering variables
    , name :: !Text
    -- ^ Estore name
    }
    deriving (Show, Eq)

instance (Ord xsource) => Estimator xsource i (Reg xsource i) where
    estimatorVariablesUsed spec =
            Set.singleton spec.lhs
            `Set.union` rhsVars
            `Set.union` conditionVars
            `Set.union` vceVars
      where
        rhsVars = getRhsVars spec.rhs
        conditionVars = maybe Set.empty getConditionVars spec.conditions
        vceVars = getVceVars spec.vce

    estimatorName = (.name)


instance HasName (Reg xsource i) where
    nameL = lens (\(Reg{name}) -> name) (\(Reg lhs rhs cond opt vce _) b -> Reg lhs rhs cond opt vce b)

instance HasLhs xsource i (Reg xsource i) where
    lhsL = lens (.lhs) (\s b -> s{lhs = b})

instance HasRhs xsource i (Reg xsource i) where
    rhsL = lens (.rhs) (\s b -> s{rhs = b})

instance HasConditions xsource i (Reg xsource i) where
    conditionsL = lens (.conditions) (\s b -> s{conditions = b})

instance HasVce xsource i (Reg xsource i) where
    vceL = lens (.vce) (\s b -> s{vce = b})

--------------------------------------------------------------------------------
-- Smart constructors                                                         --
--------------------------------------------------------------------------------

{- | Smart constructor for OLS regressions with sensible defaults.

Creates a basic regression specification with:

* Robust standard errors (use 'vce_' to change)
* No sample conditions (use 'conditions_' to add)
* No additional options

==== __Example__

@
myReg :: Reg XCoficatVar (Panel DealNumber_ AnnouncementDate_)
myReg = reg "baseline" lhsVar rhsVars
    & vce_ (ClusteredErrors clusterVars)
    & conditions_ myConditions
@
-}
reg_ :: Text                       -- ^ Model name
    -> SpecVar xsource i           -- ^ Left-hand side (dependent variable)
    -> Rhs (SpecVar xsource i)     -- ^ Right-hand side (independent variables)
    -> Reg xsource i
reg_ modelName lhsVar rhsVars = Reg
    { lhs = lhsVar
    , rhs = rhsVars
    , conditions = Nothing
    , options = Nothing
    , vce = RobustErrors
    , name = modelName
    }
