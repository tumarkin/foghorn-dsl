{- |
Module      : Foghorn.Dsl.Interpreter
Description : Project interpreters for analysis and extraction
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Provides interpreters for analyzing and extracting information from econometric
research projects. These interpreters traverse project programs to extract
metadata without executing the actual econometric operations. This module is
generally not needed by end users.


= Overview

The Interpreter module contains two main interpreters:

* __VariablesUsed__ - Extracts all variables referenced in a project
* __ModelNames__ - Extracts all model names from estimation statements

= Usage

== Extracting Variables

@
myProject :: Project XCoficatVar (Panel DealNumber_ AnnouncementDate_) ()
myProject = do
    observe_ [specVar "debug" someVar]
    estimate_ myRegression

allVars :: Set (SpecVar XCoficatVar (Panel DealNumber_ AnnouncementDate_))
allVars = variablesUsed myProject
-- Contains: someVar + all variables from myRegression
@

== Extracting Model Names

@
myEstimation :: Project XCoficatVar (Panel DealNumber_ AnnouncementDate_) ()
myEstimation = do
    estimate_ baseline
    estimate_ withControls

names :: [Text]
names = modelNames myEstimation
-- Returns: ["baseline", "withControls"]
@

= Architecture

These interpreters use Effectful's effect system to reinterpret the
'Stmt' effect into simpler effects like 'State'. This allows:

* Pure analysis without side effects
* Composition of multiple interpreters
* Type-safe extraction of project metadata

See the individual modules for detailed documentation:

* "Foghorn.Dsl.Interpreter.VariablesUsed" - Variable dependency tracking
* "Foghorn.Dsl.Interpreter.ModelNames" - Model name extraction
-}

module Foghorn.Dsl.Interpreter (
    -- * Variables Used
    -- | Extract all variables referenced in a project.
    --
    -- See "Foghorn.Dsl.Interpreter.VariablesUsed" for details.
    module Foghorn.Dsl.Interpreter.VariablesUsed,

    -- * Model Names
    -- | Extract model names from estimation statements.
    --
    -- See "Foghorn.Dsl.Interpreter.ModelNames" for details.
    module Foghorn.Dsl.Interpreter.ModelNames,
) where

import Foghorn.Dsl.Interpreter.ModelNames 
import Foghorn.Dsl.Interpreter.VariablesUsed
