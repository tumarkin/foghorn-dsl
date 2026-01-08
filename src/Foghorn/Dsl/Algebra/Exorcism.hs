{- |
Module      : Foghorn.Dsl.Algebra.Exorcism
Description : Type erasure instances for econometric variable structures
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial (see LICENSE file)
Maintainer  : https://github.com/tumarkin
Stability   : experimental
Portability : portable

Type erasure instances implementing the 'Exorcise' type class for econometric
variable types.

The 'Exorcise' type class (defined in "Foghorn.Dsl.Algebra.Var.Internal" and
exported by "Foghorn.Dsl.Algebra.Var") provides a systematic way to convert
typed structures to type-erased equivalents while preserving type information
through singleton evidence where applicable. This module provides instances for
all composite types that contain variables, enabling type erasure to work
recursively through complex structures.

==== __Type Erasure Pattern__

Most instances follow the functor pattern, using 'fmap' to recursively apply
'exorcise' to nested components:

@
instance Exorcise (Foo (EVar ...)) (Foo (XVar ...)) where
    exorcise = fmap exorcise
@

This allows type erasure to propagate automatically through aggregations,
operations, subsamples, and other composite structures.
-}
module Foghorn.Dsl.Algebra.Exorcism where

import Foghorn.Dsl.Algebra.Var.Internal
import Relude

instance
    (Exorcise (EVar xsource i d) (XVar xsource))
    => Exorcise (AggregateComp (EVar xsource i d)) (AggregateComp (XVar xsource))
    where
    exorcise = fmap exorcise

instance
    (Exorcise (EVar xsource i d) (XVar xsource))
    => Exorcise (AlgebraExp (EVar xsource i d)) (AlgebraExp (XVar xsource))
    where
    exorcise = fmap exorcise

instance
    (Exorcise (EVar xsource i d) (XVar xsource))
    => Exorcise (Subsample (EVar xsource i d)) (Subsample (XVar xsource))
    where
    exorcise = fmap exorcise

instance Exorcise (XVar xsource) (XVar xsource) where
    exorcise = id

instance Exorcise (DbFunction xvar) (DbFunction xvar) where
    exorcise = id

instance Exorcise (JoinWhere xsource i) (XJoinWhere (XVar xsource)) where
    exorcise (JoinWhere xs) = XJoinWhere . fmap exorcise $ xs

instance Exorcise (By xsource i) (XBy (XVar xsource)) where
    exorcise (By xs) = XBy xs

instance Exorcise (Peer xsource i) (XPeer (XVar xsource)) where
    exorcise (Peer op var) = XPeer op var

instance Exorcise (Having xsource i) (XHaving (XVar xsource)) where
    exorcise (Having xvars) = XHaving xvars
