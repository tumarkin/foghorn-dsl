{- |
Module      : Foghorn.Dsl.Estimator
Description : Econometric estimators for the Foghorn DSL
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

This module re-exports all available estimators in the Foghorn DSL.

= Available Estimators

* 'Reg' - Basic OLS regression without absorbed fixed effects
* 'RegHdFe' - High-dimensional fixed effects regression

= Usage

Import this module to get access to all estimators:

@
import Foghorn.Dsl.Estimator
@

Or import specific estimators:

@
import Foghorn.Dsl.Estimator.Reg
import Foghorn.Dsl.Estimator.RegHdFe
@
-}

module Foghorn.Dsl.Estimator (
    -- * Re-exports
    module Foghorn.Dsl.Estimator.Reg,
    module Foghorn.Dsl.Estimator.RegHdFe,
) where

import Foghorn.Dsl.Estimator.Reg (Reg, reg_)
import Foghorn.Dsl.Estimator.RegHdFe (RegHdFe, regHdFe_)
