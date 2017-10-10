{-# LANGUAGE TypeFamilies #-}

module V3
    ( V3 (V3)
    , (^*^)
    ) where

import Data.VectorSpace
import Data.Cross
import Data.Basis

data V3 = V3 Float Float Float deriving (Show, Eq)

instance AdditiveGroup V3 where
    zeroV = V3 0 0 0
    (V3 x1 y1 z1) ^+^ (V3 x2 y2 z2) = V3 (x1+x2) (y1+y2) (z1+z2)
    negateV (V3 x y z) = V3 (-x) (-y) (-z)

instance VectorSpace V3 where
    type Scalar V3 = Float
    c *^ (V3 x y z) = V3 (c*x) (c*y) (c*z)

instance InnerSpace V3 where
    (V3 x1 y1 z1) <.> (V3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

instance HasNormal V3 where
    normalVec v = v ^/ magnitude v

instance HasCross3 V3 where
    cross3 (V3 x1 y1 z1) (V3 x2 y2 z2) =
        V3 (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)

infixl 7 ^*^
(^*^) :: V3 -> V3 -> V3
(V3 x1 y1 z1) ^*^ (V3 x2 y2 z2) = V3 (x1*x2) (y1*y2) (z1*z2)
