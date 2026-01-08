{- |
Module      : Foghorn.Dsl.Codegen
Description : Template Haskell code generation for dataset variable accessors
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial (see LICENSE file)
Maintainer  : https://github.com/tumarkin
Stability   : experimental
Portability : portable

Provides Template Haskell utilities for generating smart constructors for
dataset variables. The 'modularize' function introspects GADT constructors
and generates accessor functions for variables in data modules.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Foghorn.Dsl.Codegen (
    modularize,
) where

import qualified Data.Char as Char
import Data.Monoid
import Foghorn.Dsl.Algebra.Var.Internal (Exorcise)
import Foghorn.Dsl.Internal.TH
import Language.Haskell.TH
import Relude hiding (Type)
{-
Given a GADT like DsfV:
> data DsfV d where
> Prc :: DsfV Float_

This is added to a table for the CoficatVar with
> data CoficatVar i d where
>    Crsp'Dsf :: DsfV d -> CoficatVar (Panel Permno_ TradingDate_) d

Then, modularize works as follows:
1. The tableConstructor name (e.g. Crsp'Dsf) is reified
2. getXDataType determines the exorcised module data type
3. getBaseDataConstrucotrs extracts the underlying GADT constructors (e.g. DsfV)
4. makeVarAccessor generates accessor functions for each constructor

For example, the Prc constructor generates:
> prc :: EVar XCoficatVar (Panel Permno_ TradingDate_) Float_
> prc = Source (Source'Dataset (exorcise (Crsp'Dsf Prc)))

-}

modularize :: Name -> Q [Dec]
modularize tableConstructor = do
    d@(DataConI _ tableGadtType tableDataType) <- reify tableConstructor

    -- Get the exorcised data type (this is the module-level exorcised
    -- algebraic data wrapper)
    xDataType <- getXDataType tableDataType

    -- Get the underlying Foghorn.Data constructors (not the module-level
    -- algebraic data wrapper)
    dataConstructors <- getBaseDataConstrucotrs tableGadtType

    -- Make the accessors
    fmap concat . mapM (makeVarAccessor tableConstructor xDataType) $ dataConstructors


makeVarAccessor
    :: Name -- ^ Table constructor type
    -> Name -- ^ Exorcised module data type
    -> Con  -- ^ GADT for base data constructor
    -> Q [Dec]
makeVarAccessor tableConstructor xDataType (GadtC (n : _) _ dGadtType) = do
    pure [signature, accessor]
  where
    signature =
        SigD accessorName
            $ itd'var
            `AppT` ConT xDataType
            `AppT` eIndices
            `AppT` eType

    Just (eIndices, eType) = extractEconIndicesAndValue dGadtType
    accessorName = mkName . firstLower $ nameBase n
    accessor = FunD accessorName [Clause [] accBody []]


    accBody :: Body
    accBody = NormalB $ source `AppE` (kdataset `AppE` ParensE (exorcise `AppE` ParensE (ConE tableConstructor `AppE` ConE n)))

    itd'var = conT_ "EVar"
    source = conE_ "Source"
    kdataset = conE_ "Source'Dataset"
    exorcise = varE_ "exorcise"

makeVarAccessor _ _ _ = pure []



getConNames :: Type -> First Name
getConNames (AppT a b) = getConNames a <> getConNames b
getConNames (ConT n) = First (Just n)
getConNames _ = First Nothing

finalConT :: Type -> Last Name
finalConT (AppT a b) = finalConT a <> finalConT b
finalConT (ConT n) = Last (Just n)
finalConT _ = Last Nothing


getBaseDataConstructor tableGadtType =
    let (ForallT _ _ sqlTypes) = tableGadtType
        Just varN = getFirst $ getConNames sqlTypes
    in varN
    
getXDataType tableDataType = do
    -- | Failure to match indicates that the datatype is not an instance of Exorcise
    (e : _) <-
        reifyInstances
            ''Exorcise
            -- [ ConT tableDataType `AppT` VarT (mkName "i") `AppT` VarT (mkName "t") `AppT` VarT (mkName "d")
            [ ConT tableDataType `AppT` VarT (mkName "i") `AppT` VarT (mkName "d")
            , VarT (mkName "b")
            ]
    let InstanceD _ _ exorciseType _ = e
        Just xDataType = getLast $ finalConT exorciseType
    pure xDataType

getBaseDataConstrucotrs tableGadtType = do
    let varN = getBaseDataConstructor tableGadtType
    TyConI dec <- reify varN
    let DataD _ _ _ _ content _ = dec
    pure content


--------------------------------------------------------------------------------
-- Utility function                                                           --
--------------------------------------------------------------------------------
firstLower :: String -> String
firstLower [] = ""
firstLower (a : as) = Char.toLower a : as


-- | Strip noise wrappers so matching is simpler.
strip :: Type -> Type
strip = \case
  ParensT t -> strip t
  SigT t _  -> strip t
  t         -> t

-- | From a result type like:
--   AppT (AppT (ConT Foghorn.Data.Crsp.Dsf.DsfV) <econIndicesPromoted>) (PromotedT Foghorn.Base.EconType.Float_)
--   return ( <econIndicesPromoted>, PromotedT Float_ ).
--
--   Works even if there’s extra nesting/parentheses/signatures.
extractEconIndicesAndValue :: Type -> Maybe (Type, Type)
extractEconIndicesAndValue = go
  where
    go = \case
      -- Match DsfV <econIndices> <econValue>
      AppT (AppT (ConT _) econIndices) econVal ->
        Just (strip econIndices, strip econVal)

      -- Be tolerant of extra application layers or wrappers
      AppT t1 t2 -> go t1 <|> go t2
      SigT t _   -> go t
      ParensT t  -> go t
      _          -> Nothing
