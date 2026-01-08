{- |
Module      : Foghorn.Dsl.Language.Quote
Description : Quasi-quoters for building specifications
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Provides quasi-quoters that simplify building econometric specifications.
These quasi-quoters allow concise syntax for specifying variables, RHS terms,
conditions, and other specification components.

= Overview

Quasi-quoters use the syntax @[quoterName| ... |]@ to embed domain-specific
syntax in Haskell code. At compile time, the quoted content is parsed and
converted to Haskell expressions.

= Available Quasi-Quoters

== Variable Lists

* 'svarQ' - Create list of named variables
* 'lhsQ' - Specify left-hand side (dependent) variable
* 'observeQ' - Mark variables for observation

== RHS Specification

* 'rhsQ' - Build right-hand side with interactions
  - Use @+@ to add variables
  - Use @##@ for factor interactions (includes individual vars)
  - Use @#@ for simple interactions (no individual vars)

== Conditions

* 'conditionsQ' - Specify sample conditions
* 'andConditionsQ' - Add AND conditions to existing spec

== Fixed Effects and Clustering

* 'absorbQ' - Specify absorbed fixed effects
* 'clusterQ' - Specify clustering variables

= Examples

@
-- Create named variables
vars = [svarQ| dealValue leverage profitability |]

-- Specify left-hand side
lhs = [lhsQ| car |]

-- Build RHS with interactions
rhs = [rhsQ| dealValue + leverage ## size |]
-- Expands to: dealValue + leverage + size + (leverage * size)

-- Specify conditions
cond = [conditionsQ| publicTarget && completedDeal |]

-- Absorbed fixed effects
absorb = [absorbQ| industry year |]

-- Clustered standard errors
vce = [clusterQ| dealNumber |]
@

-}

module Foghorn.Dsl.Language.Quote (
    -- * Specification simplification
    absorbQ,
    clusterQ,
    lhsQ,
    rhsQ,
    conditionsQ,
    observeQ,
    andConditionsQ,

    -- * Core quasi-quoters
    svarQ,

    -- * Utility functions
    setFromList,
) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Foghorn.Dsl.Internal.TH
import Foghorn.Dsl.Internal.Text
import Foghorn.Dsl.Language.Quote.Parser
import Foghorn.Dsl.Language.Types (Conditions (..), Rhs (..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Prelude as P
import Relude

{- | Quasi-quoter for absorbed fixed effects.

Specifies which variables should be absorbed (partialled out) in high-dimensional
fixed effects estimation. Variables are specified as space-separated identifiers.

==== __Example__

@
[absorbQ| industry year |]
-- Expands to: setFromList [specVar "industry" industry, specVar "year" year]
@
-}
absorbQ :: QuasiQuoter
absorbQ =
    QuasiQuoter
        { quoteExp = applyFEVars "setFromList" "absorbQ"
        , quotePat = ePat "absorbQ"
        , quoteDec = eDec "absorbQ"
        , quoteType = eTyp "absorbQ"
        }

{- | Quasi-quoter for clustered standard errors.

Specifies clustering variables for variance-covariance estimation.

==== __Example__

@
[clusterQ| dealNumber firm |]
-- Expands to: ClusteredErrors (setFromList [specVar "deal_number" dealNumber, specVar "firm" firm])
@
-}
clusterQ :: QuasiQuoter
clusterQ =
    QuasiQuoter
        { quoteExp = applyFunctionsEVars "ClusteredErrors" "setFromList" "absorbQ"
        , quotePat = ePat "absorbQ"
        , quoteDec = eDec "absorbQ"
        , quoteType = eTyp "absorbQ"
        }

{- | Quasi-quoter for left-hand side (dependent) variable.

Specifies the single dependent variable in a regression. Only one variable
should be provided.

==== __Example__

@
[lhsQ| car |]
-- Expands to: specVar "car" car
@
-}
lhsQ :: QuasiQuoter
lhsQ =
    QuasiQuoter
        { quoteExp = quoteSingle "lhs" toEVar
        , quotePat = ePat "lhsQ"
        , quoteDec = eDec "lhsQ"
        , quoteType = eTyp "lhsQ"
        }

{- | Quasi-quoter for right-hand side (independent) variables.

Supports a mini-language for specifying independent variables and their
interactions:

* @+@ - Add variables
* @##@ - Factor interaction (includes individual variables and their interaction)
* @#@ - Simple interaction (only the interaction term, no individual variables)

==== __Example__

@
[rhsQ| dealValue + leverage + profitability |]
-- Simple additive model

[rhsQ| treatmentGroup ## size |]
-- Expands to: treatmentGroup + size + (treatmentGroup * size)

[rhsQ| industry # year |]
-- Interaction only: (industry * year)
@
-}
rhsQ :: QuasiQuoter
rhsQ =
    QuasiQuoter
        { quoteExp = quoteRhs
        , quotePat = ePat "rhsQ"
        , quoteDec = eDec "rhsQ"
        , quoteType = eTyp "rhsQ"
        }

{- | Quasi-quoter for sample conditions.

Specifies filtering conditions using logical operators:

* @&&@ - Logical AND
* @||@ - Logical OR

The result is wrapped in 'Just' for use with 'conditionsL'.

==== __Example__

@
[conditionsQ| publicTarget && completedDeal |]
-- Expands to: Just (ConditionAnd (Condition publicTarget) (Condition completedDeal))
@
-}
conditionsQ :: QuasiQuoter
conditionsQ =
    QuasiQuoter
        { quoteExp = conE_ "Just" `appEQ` quoteConditions
        , quotePat = ePat "conditionsQ"
        , quoteDec = eDec "conditionsQ"
        , quoteType = eTyp "conditionsQ"
        }

{- | Quasi-quoter for adding AND conditions.

Similar to 'conditionsQ' but applies 'andCondition_' to add the condition
to an existing specification rather than replacing it.

==== __Example__

@
spec `andConditionsQ` publicTarget
-- Expands to: spec `andCondition_` (Condition publicTarget)
@
-}
andConditionsQ :: QuasiQuoter
andConditionsQ =
    QuasiQuoter
        { quoteExp = varE_ "andCondition_" `appEQ` quoteConditions
        , quotePat = ePat "conditionsQ"
        , quoteDec = eDec "conditionsQ"
        , quoteType = eTyp "conditionsQ"
        }

{- | Core quasi-quoter for creating lists of named variables.

Takes space-separated variable names and creates a list of 'SpecVar' values,
automatically converting camelCase names to snake_case for display.

This is the fundamental building block used by other quasi-quoters.

==== __Example__

@
[svarQ| dealValue marketCap leverage |]
-- Expands to:
-- [ specVar "deal_value" dealValue
-- , specVar "market_cap" marketCap
-- , specVar "leverage" leverage
-- ]
@
-}
svarQ :: QuasiQuoter
svarQ =
    QuasiQuoter
        { quoteExp = quoteMany "SpecVar" toEVar
        , quotePat = ePat "svarQ"
        , quoteDec = eDec "svarQ"
        , quoteType = eTyp "svarQ"
        }

{- | Quasi-quoter for marking variables to observe.

Generates 'observe_' calls for the specified variables, which ensures they
are included in query output for debugging even if not used in an estimation.

==== __Example__

@
[observeQ| dealValue targetSize acqSize |]
-- Expands to: observe_ [specVar "deal_value" dealValue, specVar "target_size" targetSize, specVar "acq_size" acqSize]
@
-}
observeQ :: QuasiQuoter
observeQ =
    QuasiQuoter
        { quoteExp = applyFEVars "observe_" "SpecVar"
        , quotePat = ePat "observeQ"
        , quoteDec = eDec "observeQ"
        , quoteType = eTyp "observeQ"
        }

----------------------------------------------------------------------------------------------------
-- Base quoters                                                                                   --
----------------------------------------------------------------------------------------------------
applyFEVars :: String -> String -> String -> Q Exp
applyFEVars f quoterName qq = do
    vars <- quoteMany quoterName toEVar qq
    pure $ varE_ f `AppE` ParensE vars

applyFunctionsEVars :: String -> String -> String -> String -> Q Exp
applyFunctionsEVars constructor function quoterName qq = do
    vars <- quoteMany quoterName toEVar qq
    pure $ conE_ constructor `AppE` (varE_ function `AppE` vars)

quoteRhs :: String -> Q Exp
quoteRhs = either (error . show) makeRhs . parseRhs . strip
  where
    makeRhs :: Rhs String -> Q Exp
    makeRhs (Var a) = quoteSingle "quoteRhs Var" (appConE "Var" . toEVar) a
    makeRhs (RhsCons a b) = makePair "RhsCons" a b
    makeRhs (Interact a b) = makePair "Interact" a b
    makeRhs (FactorInteract a b) = makePair "FactorInteract" a b

    appConE s = AppE (ConE $ mkName s)

    makePair :: String -> Rhs String -> Rhs String -> Q Exp
    makePair cons a b = do
        a' <- makeRhs a
        b' <- makeRhs b
        pure $ ConE (mkName cons) `AppE` a' `AppE` b'

quoteConditions :: String -> Q Exp
quoteConditions =
    either (error . show) makeCondition . parseConditions . strip
  where
    -- justCondition :: Exp -> Exp
    -- justCondition q = conE_ "Just" `AppE` q

    makeCondition :: Conditions String -> Q Exp
    makeCondition (Condition a) = quoteSingle "quoteConditions Var" (appConE "Condition" . toEVar) a
    makeCondition (ConditionAnd a b) = makePair "ConditionAnd" a b
    makeCondition (ConditionOr a b) = makePair "ConditionOr" a b
    -- makeCondition (Interact a b)       = makePair "Interact" a b
    -- makeCondition (FactorInteract a b) = makePair "FactorInteract" a b

    appConE s = AppE (ConE $ mkName s)

    makePair :: String -> Conditions String -> Conditions String -> Q Exp
    makePair cons a b = do
        a' <- makeCondition a
        b' <- makeCondition b
        pure $ ConE (mkName cons) `AppE` a' `AppE` b'

quoteSingle :: String -> (Name -> Exp) -> String -> Q Exp
quoteSingle quoterType f n = do
    names <- lookupName quoterType . strip $ n
    return . f $ names

{- | Quasi quoter using arbitrary function to convert to name
to tuple.
-}
quoteMany :: String -> (Name -> Exp) -> String -> Q Exp
quoteMany quoterType f s = do
    let requestedNames = P.words s
    names <- mapM (lookupName quoterType) requestedNames
    return . listTuplesE f $ names

-- | Convert a name to SpecVar "name" (evar Name)
toEVar :: Name -> Exp
-- toEVar n = specVar `AppE` nameStr `AppE` ParensE (evar `AppE` var)
-- toEVar n = specVar `AppE` nameStr `AppE` ParensE (evar `AppE` var)
toEVar n = evar `AppE` nameStr `AppE` var
  where
    nameStr = textLitE . toSnakeCaseName $ n
    evar = varE_ "specVar"
    var = VarE n

--------------------------------------------------------------------------------
-- Quoting utility functions                                                  --
--------------------------------------------------------------------------------
appEQ :: Exp -> (String -> Q Exp) -> String -> Q Exp
appEQ e quoter quoteString = do
    exp <- quoter quoteString
    pure $ e `AppE` exp

----------------------------------------------------------------------------------------------------
-- Utility functions                                                                              --
----------------------------------------------------------------------------------------------------
strip :: String -> String
strip = T.unpack . T.strip . T.pack

{- | Convert a list of names to a list of tuples using
the function f.
-}
listTuplesE :: (Name -> Exp) -> [Name] -> Exp
listTuplesE f = ListE . map f

-- Lookup a variable in the spliced scope
lookupName
    :: String
    -- ^ Variable type (used for error messages)
    -> String
    -- ^ Variable name
    -> Q Name
lookupName varType s =
    lookupValueName s >>= \case
        Nothing -> error . unwords $ T.pack <$> [varType, "\"" ++ s ++ "\"", "not found"]
        Just nm -> return nm

toSnakeCaseName :: Name -> Name
toSnakeCaseName = mkName . T.unpack . toSnakeCase . T.pack . nameBase

setFromList :: (Ord a) => [a] -> Set a
setFromList = Set.fromList

--------------------------------------------------------------------------------
-- Errors                                                                     --
--------------------------------------------------------------------------------
ePat t = error $ unwords [t, "quasiquoter does not handle patterns"]
eDec t = error $ unwords [t, "quasiquoter does not handle declarations"]
eTyp t = error $ unwords [t, "quasiquoter does not handle types"]
