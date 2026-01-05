#!/usr/bin/env stack
{- stack script
    --resolver lts-23.15 
    --package linear
    --package waterfall-cad
    --package waterfall-cad-svg
    --package lens
    --extra-dep waterfall-cad-0.6.0.0
    --extra-dep waterfall-cad-svg-0.6.0.0
    --extra-dep opencascade-hs-0.6.0.0
-}

import qualified Waterfall as W
import qualified Waterfall.SVG.ToSVG as W
import Linear

bedshape :: W.Solid
bedshape = let h1 = 30; l1 = 40; h2 = 21; l2 = 6.5; h3 = 19; l3 = 13.5; lfloor = 10; w = 3.2; h12 = (h1-h2)/2; h23 = (h2-h3)/2 in
    W.translate (V3 (-(h1/2)) (w/2) (l1+l2+l3+lfloor)) .W.rotate (V3 1 0 0) (pi/2) . W.prism w . W.makeShape . mconcat . reverse . snd $ foldl (\(p, arr) d -> let n = p+d in (n, W.line2D p n:arr)) (V2 0 0, []) [V2 0 (-l1), V2 h12 0, V2 0 (-l2), V2 h23 (-l3), V2 h3 0, V2 h23 l3, V2 0 l2, V2 h12 0, V2 0 l1, V2 (-h1) 0]

bedfoot :: W.Solid
bedfoot =
    (W.translate (V3 0 0 33) . W.rotate (V3 1 0 0) pi . W.uScale 30) W.unitCone
    `W.union`
    (W.translate (V3 0 0 20) . W.scale (V3 40 10 40)) W.centeredCube
    `W.union`
    (W.translate (V3 0 0 (-27)) . W.uScale 30) W.unitCone
    `W.difference`
    bedshape
    `W.difference`
    (let h = 50 in W.translate (V3 0 0 (-h)) . W.scale (V3 100 100 (2*h))) W.centeredCube

main :: IO ()
main = do
    W.writeSTL 0.001 "Bedfoot.stl" bedfoot
    W.writeDiagramSVG "Bedfoot.svg" $ W.solidDiagram (V3 1 (-1) 1) bedfoot

