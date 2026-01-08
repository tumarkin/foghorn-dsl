{- |
Module      : Foghorn.Dsl.Internal.Text
Description : Text manipulation utilities for code generation
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial (see LICENSE file)
Maintainer  : https://github.com/tumarkin
Stability   : experimental
Portability : portable

Internal utilities for formatting text, used in quasi-quoters and when
transpiling code from the DSL.
-}
module Foghorn.Dsl.Internal.Text
  ( apostrophize
  , parenthesize
  , toSnakeCase
  ) where

import qualified Data.Char as Char
import qualified Data.Text as T
import Relude

-- | Wrap text in single quotes.
--
-- >>> apostrophize "hello"
-- "'hello'"
apostrophize :: Text -> Text
apostrophize t = "'" <> t <> "'"

-- | Wrap text in parentheses.
--
-- >>> parenthesize "a + b"
-- "(a + b)"
parenthesize :: Text -> Text
parenthesize t = "(" <> t <> ")"

-- | Convert text to snake_case by inserting underscores before uppercase letters.
--
-- Each uppercase letter is converted to lowercase with a preceding underscore.
-- Leading and trailing underscores are stripped from the result.
-- Used to convert Haskell identifiers to SQL/database naming conventions.
--
-- >>> toSnakeCase "myVariableName"
-- "my_variable_name"
--
-- >>> toSnakeCase "PascalCase"
-- "pascal_case"
--
-- >>> toSnakeCase "HTTPResponse"
-- "h_t_t_p_response"
--
-- >>> toSnakeCase "_LeadingUnderscore"
-- "leading_underscore"
--
-- >>> toSnakeCase "already_snake_case"
-- "already_snake_case"
toSnakeCase :: Text -> Text
toSnakeCase = removeTrailingUnderscore . removeLeadingUnderscore . T.concatMap convertChar
  where
    convertChar c =
        let cText = T.singleton $ Char.toLower c in if Char.isUpper c then "_" <> cText else cText
    removeLeadingUnderscore t = if T.take 1 t == "_" then T.drop 1 t else t
    removeTrailingUnderscore t = if T.takeEnd 1 t == "_" then T.dropEnd 1 t else t

