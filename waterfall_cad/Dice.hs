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
import qualified Waterfall as W
import qualified Waterfall.SVG.ToSVG as W
import Linear
import Control.Lens
type Triple a = (a, a, a)

dieFaces :: [W.Solid]
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

faceSolid :: Triple (Triple Bool) -> W.Solid
faceSolid =
    let s x = if x then W.unitSphere else W.empty
        t (i, j) = W.translate (V3 (fromIntegral (i-1) * 3) (fromIntegral (j-1) * 3) 0) 
        f (i,x) = t i (s x) 
    in foldMapOf ((indexing each <.> indexing each) . withIndex) f

haskellLogo :: W.Solid
haskellLogo = 
    let paths = [
            W.pathFrom (V2 0 12)
                [ W.lineTo (V2 4 6)
                , W.lineTo (V2 0 0)
                , W.lineTo (V2 3 0)
            , W.lineTo (V2 7 6)
                , W.lineTo (V2 3 12)
                ],
            W.pathFrom (V2 4 0)
                [ W.lineTo (V2 8 6)
                , W.lineTo (V2 4 12)
                , W.lineTo (V2 7 12)
                , W.lineTo (V2 15 0)
                , W.lineTo (V2 12 0)
                , W.lineTo (V2 9.5 3.75)
                , W.lineTo (V2 7 0)
                ], 
             W.pathFrom (V2 13.66 3.5)
                [ W.lineTo (V2 12.333 5.5)
                , W.lineTo (V2 17 5.5)
                , W.lineTo (V2 17 3.5)
                ],
            W.closeLoop $ W.pathFrom (V2 11.666 6.5)
                [ W.lineTo (V2 10.333 8.5)
                , W.lineTo (V2 17 8.5)
                , W.lineTo (V2 17 6.5)
                ]
            ]
        logo = mconcat . fmap (W.prism 3 . W.makeShape . W.closeLoop) $ paths
        Just (lo, hi) = W.axisAlignedBoundingBox logo
        w = (hi - lo) ^. _x
        m = (hi + lo) / 2
    in W.uScale (7 / w ) . W.translate (negate m) $ (logo)

rotations :: [W.Solid -> W.Solid]
rotations = 
    [ id
    , W.rotate (unit _y) (pi/2)
    , W.rotate (unit _x) (pi/2)
    , W.rotate (unit _x) (-pi/2)
    , W.rotate (unit _y) (-pi/2)
    , W.rotate (unit _x) (pi)
    ]

dice :: W.Solid
dice = 
    let body = W.centeredCube `W.intersection` (W.uScale (2/3) W.unitSphere)
        positionFace = W.translate (unit _z ^* 0.5) . W.uScale 0.08
        substituteLogo = (haskellLogo :) . drop 1
        faces = mconcat $ zipWith ($) rotations (fmap positionFace . substituteLogo $ dieFaces)
    in body `W.difference` faces

main :: IO ()
main = do
    -- W.writeSTL 0.001 "dice.stl" dice
    W.writeDiagramSVG "dice.svg" $ W.solidDiagram (V3 1 1 1) $ W.rotate (V3 1 1 1) (-5*pi/6) $ W.scale (V3 100 100 100) dice
