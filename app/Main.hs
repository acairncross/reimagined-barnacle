{-# LANGUAGE NegativeLiterals #-}

module Main where

import V3
import Tracer
import Control.Monad.Trans.Reader
import Data.List (intercalate)
import Data.Array

camera = Camera (V3 0 0 -250) 480 360

theworld = World
    { surfaces =
      [ Surface
        { geometry = HalfSpace (V3 0 -200 0) (V3 0 1 0)
        , material = Material
          { matte = V3 1 1 0.2
          , gloss = V3 0 0 0
          , transparency = V3 0 0 0
          , ior = 1
          , isDielectric = False
          }
        }
      , Surface
        { geometry = HalfSpace (V3 0 0 400) (V3 0 0 -1)
        , material = Material
          { matte = V3 0.2 0.2 1
          , gloss = V3 0 0 0
          , transparency = V3 0 0 0
          , ior = 1
          , isDielectric = False
          }
        }
      , Surface
        { geometry = HalfSpace (V3 -300 0 0) (V3 1 0 0)
        , material = Material
          { matte = V3 0.2 0.2 1
          , gloss = V3 0 0 0
          , transparency = V3 0 0 0
          , ior = 1
          , isDielectric = False
          }
        }
      , Surface
        { geometry = Sphere (V3 0 -50 100) 80
        , material = Material
          { matte = V3 0 0 0
          , gloss = V3 0 0 0
          , transparency = V3 0.9 0.9 0.9
          , ior = 1.33
          , isDielectric = True
          }
        }
      , Surface
        { geometry = Sphere (V3 -100 -100 200) 80
        , material = Material
          { matte = V3 0.6 0.1 0.1
          , gloss = V3 0.2 0.2 0.2
          , transparency = V3 0 0 0
          , ior = 1.33
          , isDielectric = False
          }
        }
      , Surface
        { geometry = Sphere (V3 100 -100 200) 80
        , material = Material
          { matte = V3 0.1 0.6 0.6
          , gloss = V3 0.2 0.2 0.2
          , transparency = V3 0 0 0
          , ior = 1.33
          , isDielectric = False
          }
        }
      ]
    , lights =
      [ V3 30 300 150, V3 0 0 10, V3 -30 300 150, V3 0 -150 190, V3 400 -150 100 ]
    }

image :: Image
image = render camera theworld

toPGM :: Image -> String
toPGM img =
    "P3\n" ++
    show w ++ " " ++
    show h ++ "\n" ++
    "255\n" ++
    intercalate "\n" [
        intercalate " " [
            showV3 (img ! (j,i)) | i <- [1..w]
        ] | j <- [1..h]
    ]
  where
    (h,w) = snd (bounds img)
    showV3 (V3 x y z) = intercalate " " (map showComponent [x, y, z])
    showComponent = show . toInteger . floor . sqrt . (* (255*255)) . (min 1)

main :: IO ()
main = putStr (toPGM image)
