{- |
Module      : Foghorn.Dsl.Interpreter.VariablesUsed
Description : Extract variables used in research projects
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Provides functionality for extracting all variables used in a research project.
This ensures that generating data management processes are complete. This module
is generally not needed by end users.

= Usage

@
myProject :: Project XCoficatVar (Panel DealNumber_ AnnouncementDate_) ()
myProject = do
    [observeQ| debugVar |]
    estimate_ myRegression

allVars :: Set (SpecVar XCoficatVar (Panel DealNumber_ AnnouncementDate_))
allVars = variablesUsed myProject
-- Contains: debugVar + all variables from myRegression
@
-}

{-# LANGUAGE UndecidableInstances #-}

module Foghorn.Dsl.Interpreter.VariablesUsed (
    -- * Project variables
    variablesUsed,
) where

import qualified Data.Set as Set
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Foghorn.Dsl.Language.Types.Spec.Estimator (Estimator (estimatorVariablesUsed))
import Foghorn.Dsl.Language.Types.SpecVar (SpecVar)
import Foghorn.Dsl.Language.Statement (Stmt (..))
import Relude hiding (execState, State, modify)

----------------------------------------------------------------------------------------------------
-- Variables used interpreter                                                                     --
----------------------------------------------------------------------------------------------------

{- | Extract all variables used in a research project.

Interprets a project program and collects all variables that appear in:

* Estimators (via 'estimatorVariablesUsed')
* Observe statements

Other statement types (comments, headers, etc.) do not contribute variables.
-}
variablesUsed
    :: forall xsource i r a
     . (Ord xsource)
    => Eff '[Stmt xsource i] a
    -> Set (SpecVar xsource i)
variablesUsed = runPureEff . variablesUsedEff

variablesUsedEff
    :: forall xsource i es a
     . (Ord xsource)
    => Eff (Stmt xsource i ': es) a
    -> Eff es (Set (SpecVar xsource i))
variablesUsedEff = reinterpret_ (execState Set.empty) asVarsUsed
  where
    asVarsUsed
        :: forall localEs b. ()
        => Stmt xsource i (Eff localEs) b
        -> Eff (State (Set (SpecVar xsource i)) : es) b
    asVarsUsed = \case
        BlankLine__ -> pure ()
        Comment__ {} -> pure ()
        Divider__ -> pure ()
        Estimate__ spec -> modify (Set.union $ estimatorVariablesUsed spec)
        Table__ {} -> pure ()
        Header__ {} -> pure ()
        Observe__ var -> modify (Set.insert var)
        ReadCsv__ {} -> pure ()
        Raw__ {} -> pure ()
       
