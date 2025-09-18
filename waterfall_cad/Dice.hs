#!/usr/bin/env stack
{- stack script --resolver lts-23.15 
    --package linear
    --package waterfall-cad
    --package waterfall-cad-svg
    --package lens
    --extra-dep waterfall-cad-0.6.0.0
    --extra-dep waterfall-cad-svg-0.6.0.0
    --extra-dep opencascade-hs-0.6.0.0
-}

-- short-description: Haskell Dice
--
-- description: Die intended as a give-away at a conference
--
-- image: https://doscienceto.it/blog/photos/haskell-dice-01.jpg
import qualified Waterfall
import Waterfall.SVG.ToSVG (writeDiagramSVG)
import Data.Function ((&))
import Linear
import Control.Lens
type Triple a = (a, a, a)

dieFaces :: [Waterfall.Solid]
dieFaces = fmap faceSolid . over (traverse . each . each) (== 1) $ [
   ((0, 0, 0),
    (0, 1, 0),
    (0, 0, 0)),

   ((1, 0, 0), 
    (0, 0, 0), 
    (0, 0, 1)), 

   ((1, 0, 0), 
    (0, 1, 0), 
    (0, 0, 1)), 

   ((1, 0, 1), 
    (0, 0, 0), 
    (1, 0, 1)), 

   ((1, 0, 1), 
    (0, 1, 0), 
    (1, 0, 1)), 

   ((1, 0, 1), 
    (1, 0, 1), 
    (1, 0, 1))]

faceSolid :: Triple (Triple Bool) -> Waterfall.Solid
faceSolid =
    let s x = if x then Waterfall.unitSphere else Waterfall.empty
        t (i, j) = Waterfall.translate (V3 (fromIntegral (i-1) * 3) (fromIntegral (j-1) * 3) 0) 
        f (i,x) = t i (s x) 
    in foldMapOf ((indexing each <.> indexing each) . withIndex) f

haskellLogo :: Waterfall.Solid
haskellLogo = 
    let paths = [
            Waterfall.pathFrom (V2 0 12)
                [ Waterfall.lineTo (V2 4 6)
                , Waterfall.lineTo (V2 0 0)
                , Waterfall.lineTo (V2 3 0)
            , Waterfall.lineTo (V2 7 6)
                , Waterfall.lineTo (V2 3 12)
                ],
            Waterfall.pathFrom (V2 4 0)
                [ Waterfall.lineTo (V2 8 6)
                , Waterfall.lineTo (V2 4 12)
                , Waterfall.lineTo (V2 7 12)
                , Waterfall.lineTo (V2 15 0)
                , Waterfall.lineTo (V2 12 0)
                , Waterfall.lineTo (V2 9.5 3.75)
                , Waterfall.lineTo (V2 7 0)
                ], 
             Waterfall.pathFrom (V2 13.66 3.5)
                [ Waterfall.lineTo (V2 12.333 5.5)
                , Waterfall.lineTo (V2 17 5.5)
                , Waterfall.lineTo (V2 17 3.5)
                ],
            Waterfall.closeLoop $ Waterfall.pathFrom (V2 11.666 6.5)
                [ Waterfall.lineTo (V2 10.333 8.5)
                , Waterfall.lineTo (V2 17 8.5)
                , Waterfall.lineTo (V2 17 6.5)
                ]
            ]
        logo = mconcat . fmap (Waterfall.prism 3 . Waterfall.makeShape . Waterfall.closeLoop) $ paths
        Just (lo, hi) = Waterfall.axisAlignedBoundingBox logo
        w = (hi - lo) ^. _x
        m = (hi + lo) / 2
    in Waterfall.uScale (7 / w ) . Waterfall.translate (negate m) $ (logo)

rotations :: [Waterfall.Solid -> Waterfall.Solid]
rotations = 
    [ id
    , Waterfall.rotate (unit _y) (pi/2)
    , Waterfall.rotate (unit _x) (pi/2)
    , Waterfall.rotate (unit _x) (-pi/2)
    , Waterfall.rotate (unit _y) (-pi/2)
    , Waterfall.rotate (unit _x) (pi)
    ]

dice :: Waterfall.Solid
dice = 
    let body = Waterfall.centeredCube `Waterfall.intersection` (Waterfall.uScale (2/3) Waterfall.unitSphere)
        positionFace = Waterfall.translate (unit _z ^* 0.5) . Waterfall.uScale 0.08
        substituteLogo = (haskellLogo :) . drop 1
        faces = mconcat $ zipWith ($) rotations (fmap positionFace . substituteLogo $ dieFaces)
    in body `Waterfall.difference` faces

main :: IO ()
main = do
    Waterfall.writeSTL 0.001 "dice.stl" dice
