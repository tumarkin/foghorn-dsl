{- |
Module      : Foghorn.Dsl.Internal.TH
Description : Template Haskell helpers for constructing AST nodes
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial (see LICENSE file)
Maintainer  : https://github.com/tumarkin
Stability   : experimental
Portability : portable

Internal Template Haskell utilities for constructing AST nodes from strings.
These utilities reduce boilerplate when writing Template Haskell metaprograms
that generate variable accessors and quasi-quoters.
-}
module Foghorn.Dsl.Internal.TH where

import Language.Haskell.TH
import Relude hiding (Type)

-- | Construct a constructor expression from a string name.
--
-- @conE_ "Just"@ produces @ConE (mkName "Just")@
conE_ :: String -> Exp
conE_ = ConE . mkName

-- | Construct a constructor type from a string name.
--
-- @conT_ "Maybe"@ produces @ConT (mkName "Maybe")@
conT_ :: String -> Type
conT_ = ConT . mkName

-- | Construct a variable expression from a string name.
--
-- @varE_ "x"@ produces @VarE (mkName "x")@
varE_ :: String -> Exp
varE_ = VarE . mkName

-- | Construct a type variable from a string name.
--
-- @varT_ "a"@ produces @VarT (mkName "a")@
varT_ :: String -> Type
varT_ = VarT . mkName

-- | Construct a variable pattern from a string name.
--
-- @varP_ "x"@ produces @VarP (mkName "x")@
varP_ :: String -> Pat
varP_ = VarP . mkName

-- | Construct a function declaration from a string name and clauses.
--
-- @funD_ "myFunc" [clause]@ produces @FunD (mkName "myFunc") [clause]@
funD_ :: String -> [Clause] -> Dec
funD_ = FunD . mkName

-- | Construct a string literal expression from a string.
--
-- @litE_ "hello"@ produces @LitE (StringL "hello")@
litE_ :: String -> Exp
litE_ = LitE . StringL

-- | Convert a single character to a string (singleton list).
charToString :: Char -> String
charToString = (: [])

-- | Create a typed Text literal expression from a Name.
--
-- Extracts the base name of the Name, creates a string literal from it,
-- and adds a type signature indicating it's Text.
--
-- Used when generating Text constants in spliced code.
textLitE :: Name -> Exp
textLitE n = SigE (LitE . StringL . nameBase $ n) textTE
  where
    textTE :: Type
    textTE = ConT . mkName $ "Text"
