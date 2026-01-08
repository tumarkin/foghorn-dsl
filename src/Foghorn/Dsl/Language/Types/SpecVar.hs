{- |
Module      : Foghorn.Dsl.Language.Types.SpecVar
Description : Named econometric variables for specifications
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Provides 'SpecVar', a wrapper that pairs an econometric variable with its name.
This is used when building specifications (regressions, summary statistics) where
variables need both their computed value and a display name for output.

The module erases the type-level data type information while preserving the
index structure, allowing heterogeneous collections of variables with the same
indices but different data types.

= Usage

__Note:__ Direct use of 'specVar' is generally not necessary. Users should use
the quasi-quoters from "Foghorn.Dsl.Language.Quote" (such as 'svarQ',
'lhsQ', 'rhsQ') which provide more convenient syntax and handle variable naming
automatically.

The 'specVar' function is provided for advanced use cases, library development,
and situations where quasi-quoters cannot be used.
-}
module Foghorn.Dsl.Language.Types.SpecVar
    ( SpecVar(..)
    , specVar
    ) where

import Data.Singletons
import Foghorn.Dsl.Algebra.Var.Internal
import Relude

{- | A named econometric variable.

Pairs an econometric variable with its display name. The type-level data type
@d@ is erased (via 'exorcise'), allowing variables of different types to be
collected together as long as they share the same index structure @i@.

This is essential for specifications where you need a collection of variables
like @[SpecVar xsource i]@ that might include floats, indicators, dates, etc.

==== __Fields__

* @xvar@ - The underlying econometric variable (type-erased)
* @name@ - Display name for output (e.g., "log_assets", "market_cap")
-}
data SpecVar xsource i = SpecVar
    { xvar :: !(XVar xsource)
    , name :: !Text
    }
    deriving (Show, Eq, Ord)

{- | Create a named econometric variable.

Pairs an econometric variable with a display name, erasing its type-level
data type information in the process. The index structure is preserved in
the type signature.

__Note:__ Direct use of this function is generally not necessary. Prefer using
quasi-quoters from "Foghorn.Dsl.Language.Quote" such as:

* 'svarQ' - For creating lists of named variables
* 'lhsQ' - For left-hand side variables
* 'rhsQ' - For right-hand side variables

The quasi-quoters handle variable naming automatically (converting to snake_case)
and provide more convenient syntax.

==== __Example__ (Advanced Usage)

@
-- Direct creation (not typical usage)
assets :: SpecVar XCoficatVar (Panel DealNumber_ AnnouncementDate_)
assets = specVar "log_assets" (ln_ Funda.at)

-- Prefer quasi-quoters instead:
-- [lhsQ| car |]  instead of: specVar "car" car
-- [rhsQ| leverage + profitability |]  instead of manually constructing
@
-}
specVar
    :: forall xsource i d
     . (SingI i, SingI d)
    => Text
    -> EVar xsource i d
    -> SpecVar xsource i
specVar n v = SpecVar{name = n, xvar = exorcise v}
