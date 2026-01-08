{- |
Module      : Foghorn.Dsl.Language.Quote.Parser
Description : Parsers for quasi-quoter mini-languages
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial use only
Maintainer  : https://github.com/tumarkin
Stability   : experimental

Provides Megaparsec-based parsers for the mini-languages used in specification
quasi-quoters. These parsers convert string representations of RHS terms and
conditions into typed ASTs.

This module is used internally by "Foghorn.Dsl.Language.Quote" and is not
typically imported directly by users.

= Supported Languages

== RHS Language

Parses right-hand side specifications with operators:

* @+@ - Variable addition (lowest precedence)
* @#@ - Simple interaction
* @##@ - Factor interaction (highest precedence)

Parentheses can be used for grouping.

== Conditions Language

Parses logical conditions with operators:

* @&&@ - Logical AND
* @||@ - Logical OR

Parentheses can be used for grouping.
-}

module Foghorn.Dsl.Language.Quote.Parser (
    parseRhs,
    parseRhsT,
    parseConditions,
    parseConditionsT,
) where

import Control.Monad.Combinators.Expr
import Foghorn.Dsl.Language.Types
import Relude
import Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as X

{- | Parse RHS expression string.

Parses a right-hand side specification into an 'Rhs' AST. Returns either
a parse error or the parsed structure.

==== __Example__

@
parseRhs "dealValue + leverage ## size"
-- Right (RhsCons (Var "dealValue") (FactorInteract (Var "leverage") (Var "size")))
@
-}
parseRhs :: String -> Either (ParseErrorBundle String Void) (Rhs String)
parseRhs = parse (rhsExpr <* eof) "Parsing RHS"

{- | Parse RHS expression in a monadic context.

Monad transformer version of 'parseRhs' for use in Template Haskell splices.
-}
parseRhsT
    :: (Monad m)
    => String
    -> m (Either (ParseErrorBundle String Void) (Rhs String))
parseRhsT = runParserT (rhsExpr <* eof) "Parsing RHS"

{- | Parse conditions expression string.

Parses a logical conditions specification into a 'Conditions' AST. Returns
either a parse error or the parsed structure.

==== __Example__

@
parseConditions "publicTarget && completedDeal"
-- Right (ConditionAnd (Condition "publicTarget") (Condition "completedDeal"))
@
-}
parseConditions :: String -> Either (ParseErrorBundle String Void) (Conditions String)
parseConditions = parse (conditionsExpr <* eof) "Parsing Conditions"

{- | Parse conditions expression in a monadic context.

Monad transformer version of 'parseConditions' for use in Template Haskell splices.
-}
parseConditionsT
    :: (Monad m)
    => String
    -> m (Either (ParseErrorBundle String Void) (Conditions String))
parseConditionsT = runParserT (conditionsExpr <* eof) "Parsing Conditions"

----------------------------------------------------------------------------------------------------
-- Types                                                                                          --
----------------------------------------------------------------------------------------------------
type ParserT = ParsecT Void String

----------------------------------------------------------------------------------------------------
-- Rhs parser                                                                                     --
----------------------------------------------------------------------------------------------------
rhsExpr :: ParserT m (Rhs String)
rhsExpr = makeExprParser term table <?> "expression"

term :: ParserT m (Rhs String)
term = parens rhsExpr <|> varIdentifier <?> "term"

varIdentifier :: ParserT m (Rhs String)
varIdentifier = Var <$> identifier

table :: [[Operator (ParserT m) (Rhs String)]]
table =
    [ [binary "##" FactorInteract]
    , [binary "#" Interact]
    , [binary "+" RhsCons]
    ]

binary :: String -> (a -> a -> a) -> Operator (ParserT m) a
binary name f = InfixL (f <$ symbol name)

--------------------------------------------------------------------------------
-- Conditions parser                                                          --
--------------------------------------------------------------------------------
conditionsExpr :: ParserT m (Conditions String)
conditionsExpr = makeExprParser condTerm condTable <?> "expression"

condTerm :: ParserT m (Conditions String)
condTerm = parens conditionsExpr <|> condIdentifier <?> "term"

condIdentifier :: ParserT m (Conditions String)
condIdentifier = Condition <$> identifier

condTable :: [[Operator (ParserT m) (Conditions String)]]
condTable =
    [
        [ binary "&&" ConditionAnd
        , binary "||" ConditionOr
        ]
    ]

----------------------------------------------------------------------------------------------------
-- Utility parsers                                                                                --
----------------------------------------------------------------------------------------------------
parens :: ParserT m a -> ParserT m a
parens = between (symbol "(") (symbol ")")

whiteSpace :: ParserT m ()
whiteSpace = X.space space1 empty empty

symbol :: Tokens String -> ParserT m String
symbol = X.symbol whiteSpace

identifier :: ParserT m String
identifier = MP.some (letterChar <|> char '\'' <|> digitChar) <* whiteSpace
