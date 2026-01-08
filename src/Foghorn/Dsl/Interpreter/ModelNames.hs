{- |
Module      : Foghorn.Dsl.Interpreter.ModelNames
Description : Extract model names from research projects
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Provides functionality for extracting model names from a research project.
This is useful for coordinating estimation and output phases in statistical
software. This module is generally not needed by end users.

= Usage

@
myProject :: Project XCoficatVar (Panel DealNumber_ AnnouncementDate_) ()
myProject = do
    estimate_ regression1
    estimate_ regression2

modelNames :: [Text]
modelNames = modelNames myProject
-- Returns: ["regression1", "regression2"]
@
-}

{-# LANGUAGE UndecidableInstances #-}

module Foghorn.Dsl.Interpreter.ModelNames (
    modelNames,
) where

import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Foghorn.Dsl.Language.Types
import Relude hiding (State, execState, modify)

----------------------------------------------------------------------------------------------------
-- Model names interpreter                                                                        --
----------------------------------------------------------------------------------------------------

{- | Extract all model names from a research project.

Interprets a project program and collects the names of all estimators
that appear in 'estimate_' statements. The order of names matches the order
of estimation in the project.

This is used by 'tableDisplay_' and 'tableSave_' to generate the list of model
names for the table output.
-}
modelNames
    :: forall xsource i es a
     . ()
    => Eff (Stmt xsource i ': es) a
    -> Eff es [Text]
modelNames =
    fmap toList . reinterpret_ (execState Seq.empty) asModelNames
  where
    asModelNames
        :: forall localEs b. ()
        => Stmt xsource i (Eff localEs) b
        -> Eff (State (Seq Text) : es) b
    asModelNames = \case
        BlankLine__ -> pure ()
        Comment__ {} -> pure ()
        Divider__ -> pure ()
        Estimate__ spec -> modify (|> estimatorName spec)
        Table__ {} -> pure ()
        Header__ {} -> pure ()
        Observe__ {} -> pure ()
        ReadCsv__ {} -> pure ()
        Raw__ {} -> pure ()

