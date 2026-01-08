{- |
Module      : Foghorn.Dsl.Language.Statement
Description : Effectful effect for research project statements
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Defines the 'Stmt' Effectful effect GADT that represents atomic statements
in a research project. This is the low-level representation used internally by
the project DSL.

Most users will not interact with this module directly. Instead, use the
high-level functions from "Foghorn.Dsl.Language" like 'estimate_',
'observe_', 'comment_', etc.

The 'makeEffect' Template Haskell call generates smart constructors for each
statement type (suffixed with @__@).
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Foghorn.Dsl.Language.Statement where

import Foghorn.Dsl.Language.Types.SpecVar
import Foghorn.Dsl.Language.Types.Spec.Estimator (Estimator)
import Effectful.TH
import Relude

{- | Research project statement effect.

Represents the atomic operations that can appear in a research project.
This is an Effectful effect GADT where each constructor corresponds to
a different kind of statement.

The type parameters are:

* @xsource@ - Variable source (e.g., 'XCoficatVar')
* @i@ - Index structure (e.g., 'Panel DealNumber_ AnnouncementDate_')
* @m@ - Monadic context (managed by Effectful)
* @a@ - Return type of the statement

==== __Constructors__

* 'BlankLine__' - Insert a blank line
* 'Comment__' - Add a comment
* 'Divider__' - Insert a divider line
* 'Estimate__' - Run an estimator
* 'Table__' - Output estimation results table
* 'Header__' - Add a section header
* 'Observe__' - Display variable values for debugging
* 'ReadCsv__' - Load CSV data
* 'Raw__' - Insert raw code
-}
data Stmt xsource i m a where
    BlankLine__ :: Stmt xsource i m ()
    Comment__ :: Text -> Stmt xsource i m ()
    Divider__ :: Stmt xsource i m ()
    Estimate__ :: Estimator xsource i a => a -> Stmt xsource i m ()
    Table__ :: Maybe FilePath -> [Text] -> Stmt xsource i m ()
    Header__ :: Text -> Stmt xsource i m ()
    Observe__ :: SpecVar xsource i -> Stmt xsource i m ()
    ReadCsv__ :: Text -> Stmt xsource i m ()
    Raw__ :: Text -> Stmt xsource i m ()

-- | Generate smart constructors for each statement type.
makeEffect ''Stmt
