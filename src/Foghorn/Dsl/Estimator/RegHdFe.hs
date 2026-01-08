{- |
Module      : Foghorn.Dsl.Estimator.RegHdFe
Description : High-dimensional fixed effects regression estimator
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Provides the 'RegHdFe' type for regressions with absorbed fixed effects,
which efficiently handles high-dimensional fixed effects.

The estimator supports:

* Left-hand side (dependent) variable
* Right-hand side (independent) variables with interactions
* Absorbed fixed effects (partialled out)
* Sample conditions (if clauses)
* Variance-covariance specifications (robust, clustered, or unadjusted errors)
* Additional options

= Usage

Typically constructed using the smart constructor and lens operations:

@
myRegHdFe :: RegHdFe XCoficatVar (Panel DealNumber_ AnnouncementDate_)
myRegHdFe = regHdFe_ "baseline" [lhsQ| car |] [rhsQ| dealValue + leverage |] [absorbQ| industry year |]
    & vce_ (ClusteredErrors [clusterQ| dealNumber |])
    & conditions_ [conditionsQ| publicTarget |]
@
-}

module Foghorn.Dsl.Estimator.RegHdFe (
    -- * Estimators
    RegHdFe (..),

    -- * Smart constructors
    regHdFe_,

) where

import qualified Data.Set as Set
import Foghorn.Dsl.Language.Types
import Relude
import Relude.Extra.Lens (lens, Lens')
import Foghorn.Dsl.Estimator.Reg 

--------------------------------------------------------------------------------
-- Estimation methods                                                          --
--------------------------------------------------------------------------------

{- | High-dimensional fixed effects regression specification.

Represents a regression with absorbed fixed effects. The absorbed variables are
partialled out efficiently without estimating individual coefficients.

==== __Fields__

* @lhs@ - Dependent variable
* @rhs@ - Independent variables (may include interactions)
* @conditions@ - Optional sample restrictions (if clauses)
* @options@ - Additional options
* @absorb@ - Fixed effects to absorb (partial out)
* @vce@ - Variance-covariance estimation method
* @name@ - Model name for storage and output tables

==== __Instances__

* 'Estimator' - Full estimator interface (variables used, name extraction)
* 'HasName' - Access/modify the model name
* 'HasLhs' - Access/modify left-hand side variable
* 'HasRhs' - Access/modify right-hand side variables
* 'HasConditions' - Access/modify sample conditions
* 'HasVce' - Access/modify variance-covariance specification
* 'HasAbsorb' - Access/modify absorbed fixed effects
-}
data RegHdFe xsource i = RegHdFe
    { reg :: !(Reg xsource i)
    -- ^ Base regression specification (lhs, rhs, conditions, vce, options, name)
    , absorb :: !(Set (SpecVar xsource i))
    -- ^ Fixed effects to absorb (partial out)
    }
    deriving (Show, Eq)



regL :: Lens' (RegHdFe xsource i) (Reg xsource i)
regL = lens (.reg) (\s b -> s{reg = b})

instance (Ord xsource) => Estimator xsource i (RegHdFe xsource i) where
    estimatorVariablesUsed spec =
            estimatorVariablesUsed spec.reg
            `Set.union` spec.absorb

    estimatorName = (.reg.name)

instance HasName (RegHdFe xsource i) where
    nameL = regL . nameL

instance HasLhs xsource i (RegHdFe xsource i) where
    lhsL = regL . lhsL

instance HasRhs xsource i (RegHdFe xsource i) where
    rhsL = regL . rhsL

instance HasConditions xsource i (RegHdFe xsource i) where
    conditionsL = regL  . conditionsL

instance HasVce xsource i (RegHdFe xsource i) where
    vceL = regL . vceL

instance HasAbsorb xsource i (RegHdFe xsource i) where
    absorbL = lens (.absorb) (\s b -> s{absorb = b})

--------------------------------------------------------------------------------
-- Smart constructors                                                         --
--------------------------------------------------------------------------------

{- | Smart constructor for high-dimensional fixed effects regressions.

Creates a RegHdFe specification with sensible defaults:

* Robust standard errors (use 'vce_' to change)
* No sample conditions (use 'conditions_' to add)
* No additional options

This constructor avoids the nested Reg structure, providing a clean interface
that can be customized using helper functions like 'vce_', 'conditions_', and 'absorb_'.

==== __Example__

@
myRegHdFe :: RegHdFe XCoficatVar (Panel DealNumber_ AnnouncementDate_)
myRegHdFe = regHdFe_ "baseline" lhsVar rhsVars absorbVars
    & vce_ (ClusteredErrors clusterVars)
    & conditions_ myConditions
    & absorb_ additionalFE
@
-}
regHdFe_ :: Text                        -- ^ Model name
        -> SpecVar xsource i            -- ^ Left-hand side (dependent variable)
        -> Rhs (SpecVar xsource i)      -- ^ Right-hand side (independent variables)
        -> Set (SpecVar xsource i)      -- ^ Fixed effects to absorb
        -> RegHdFe xsource i
regHdFe_ modelName lhsVar rhsVars absorbVars = RegHdFe
    { reg = Reg
        { lhs = lhsVar
        , rhs = rhsVars
        , conditions = Nothing
        , options = Nothing
        , vce = RobustErrors
        , name = modelName
        }
    , absorb = absorbVars
    }


