{-# LANGUAGE TupleSections #-}

module Tracer
    ( render
    , Camera (..)
    , World (..)
    , Surface (..)
    , Geometry (..)
    , Material (..)
    , Image
    ) where

import Control.Monad.Trans.Reader
import Data.Array
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.VectorSpace

import Debug.Trace

import V3

type Image = Array (Int,Int) RGB

data Camera = Camera Point Int Int

data World = World
    { surfaces :: [Surface]
    , lights :: [Point]
    }

data Surface = Surface
    { geometry :: Geometry
    , material :: Material
    }

data Material = Material
    { matte :: V3
    , gloss :: V3
    , transparency :: V3
    , ior :: Float
    , isDielectric :: Bool
    }

data Geometry
    = Sphere Point Float        -- Center and radius
    | HalfSpace Point Direction -- Point on plane and (unit) outwards normal

type Ray = (Point, Direction)
type Plane = (Point, Direction)

type Direction = V3
type Point = V3
type RGB = V3

ε = 0.001 :: Float

maxDepth = 5 :: Int

render :: Camera -> World -> Image
render (Camera o w h) world =
    listArray ((1,1), (h,w))
        [ let (x,y) = toWorld (i,j)
          in runReader (raytrace (o, normalized ((V3 x y 0) ^-^ o)) 0) world
        | j <- [1..h], i <- [1..w]
        ]
  where
    toWorld (i,j) = (fromIntegral (i - w`div`2), fromIntegral (h`div`2 - j))

raytrace :: Ray -> Int -> Reader World RGB
raytrace (o,u) depth = do
    xm <- intersectWorld1 (o,u)
    case xm of
        Nothing -> return zeroV
        Just (s,p,n) -> do
            let mat = material s
                εn = ε*^n
                η = ior mat                      
                rr0 = ((1-η)/(1+η))**2           
                rr = rr0 + (1-rr0) * (1+u<.>n)**2
            diff <- traceShads (o,u) (s,p^+^εn,n)
            refl <- if depth < maxDepth
                then raytrace (p^+^εn, negateV (reflectV u n)) (depth+1)
                else return zeroV
            refr <- if depth < maxDepth
                then case refractV u n (1/ior mat) of
                    Nothing -> return zeroV
                    Just v -> traceTrans (p^-^εn,v) depth
                else return zeroV
            return $ matte mat ^*^ diff
                ^+^ gloss mat ^*^ refl
                ^+^ if isDielectric mat
                    then rr *^ refl ^+^ (1-rr) *^ (transparency mat ^*^ refr)
                    else zeroV


traceTrans :: Ray -> Int -> Reader World RGB
traceTrans (o,u) depth = do
    xm <- intersectWorld1 (o,u)
    case xm of
        Nothing -> return zeroV
        Just (s,p,n) -> case refractV u (negateV n) (ior (material s)) of
            Nothing -> return zeroV
            Just v -> raytrace (p^+^ε*^normalized n,v) (depth+1)

reflectV :: Direction -> Direction -> Direction
reflectV u n = (2*(u<.>n))*^n ^-^ u

reflectRay :: Direction -> Direction -> Direction
reflectRay u n = negateV (reflectV u n)

refractV :: Direction -> Direction -> Float -> Maybe Direction
refractV u n η =
    let c1 = -u<.>n
        cs2 = 1 - η**2 * (1-c1**2)
    in if cs2 < 0
        then Nothing
        else Just $ η*^u ^+^ (η*c1-sqrt cs2)*^n

traceShads :: Ray -> (Surface, Point, Direction) -> Reader World RGB
traceShads (o,u) (s,p,n) = do
    world <- ask
    fmap sumV (sequence (map traceShad (lights world)))
  where
    traceShad :: Point -> Reader World RGB
    traceShad q = do
        let l = normalized (q^-^p)
            diff = (l<.>n) *^ (V3 1 1 1)--  *^ (iDiff (material s)) -- assumes white light
            -- r = reflectV l n
            -- v = negateV u
            -- spec = (r<.>v) ** (shininess (material s)) -- assumes white light
        xm <- intersectWorld1 (p,l)
        return $ case xm of
            Nothing -> diff --  ^+^ spec
            Just (_,q',_) -> if magnitude (q^-^p) < magnitude (q'^-^p)
                then diff --  ^+^ spec
                else zeroV

intersectWorld :: Ray -> Reader World [(Surface, Point, Direction)]
intersectWorld ray =
    ask >>= \world ->
        return $ catMaybes [ fmap (tupCons surf) (intersectSurface ray surf)
                           | surf <- surfaces world ]
  where
    tupCons x (y,z) = (x,y,z)

intersectWorld1 :: Ray -> Reader World (Maybe (Surface, Point, Direction))
intersectWorld1 (o,l) = do
    xs <- intersectWorld (o,l)
    return $ if length xs == 0
        then Nothing
        else Just $ minimumBy cmp xs
  where
    dist (_,p,n) = magnitude (p^-^o)
    cmp x1 x2 = dist x1 `compare` dist x2

intersectGeometry :: Ray -> Geometry -> Maybe (Point, Direction)

intersectGeometry (o,u) (Sphere c r)
    | discriminant > 0 && dMinus > 0 =
        let p = o ^+^ (dMinus*^u) in Just (p, normalized (p^-^c))
    | discriminant > 0 && dPlus > 0 =
        let p = o ^+^ (dPlus*^u) in Just (p, normalized (p^-^c))
    | otherwise = Nothing
  where
    discriminant = (u<.>(o^-^c))**2 - (magnitude (o^-^c))**2 + r**2
    dMinus = -(u<.>(o^-^c)) - sqrt discriminant
    dPlus = -(u<.>(o^-^c)) + sqrt discriminant

intersectGeometry (o,u) (HalfSpace p n)
    | d > 0 = let q = o ^+^ (d*^u) in Just (q,n)
    | otherwise = Nothing
  where
    d = ((p^-^o)<.>n) / (u<.>n)

intersectSurface :: Ray -> Surface -> Maybe (Point, Direction)
intersectSurface (o,u) surf = intersectGeometry (o,u) (geometry surf)
