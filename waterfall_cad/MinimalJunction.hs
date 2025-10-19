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


knobI :: Double -> W.Solid
knobI w = W.uScale w . W.rotate (V3 1 0 0) (pi/2) . W.translate (V3 0 0 (-0.5)) $
    (W.translate (V3 0 0 0.125) . W.uScale 0.25) W.unitCone
    `W.union`
    (W.translate (V3 0 0 0.875) . W.rotate (V3 1 0 0) pi . W.uScale 0.25) W.unitCone
    `W.union`
    (W.translate (V3 0 0 0.375) . W.scale (V3 0.5 0.5 0.25)) W.unitCylinder
    `W.union`
    (W.translate (V3 (-0) (-0.5) 0.375) . W.scale (V3 0.5 1 0.25)) W.unitCube

knobO :: Double -> W.Solid
knobO w = W.uScale w . W.rotate (V3 1 0 0) (pi/2) . W.translate (V3 0 0 (-0.5)) $
    (W.translate (V3 0 0 0) . W.scale (V3 0.5 0.5 1)) W.unitCylinder
    `W.union`
    (W.translate (V3 (-0) (-0.5) 0) . W.scale (V3 0.5 1 1)) W.unitCube
    `W.difference`
    (W.translate (V3 (-0.5) (-0.5) (0.375)) . W.scale (V3 1 1 0.25)) W.unitCube
    `W.difference`
    (W.translate (V3 0 0 0.125) . W.uScale 0.25) W.unitCone
    `W.difference`
    (W.translate (V3 0 0 0.875) . W.rotate (V3 1 0 0) pi . W.uScale 0.25) W.unitCone
    `W.difference`
    let h = 0.5 in
    (W.translate (V3 (-0.5) 0 0.5) . W.rotate (V3 1 0 0) (pi/4) . W.scale (V3 0.5 (h/sqrt 2) (h/sqrt 2)) . W.translate (V3 0 (-0.5) (-0.5))) W.unitCube

junctionI :: Double -> Double -> W.Solid
junctionI l w =
    (W.translate (V3 (w/2) 0 0) . W.scale (V3 (l-w) w w)) W.centeredCube
    `W.union`
    (W.translate (V3 ((w-l)/2) 0 0)) (knobI w)

junctionO :: Double -> Double -> W.Solid
junctionO l w = let d = l/2-w in
    (W.translate (V3 (-w/2) 0 0) . W.scale (V3 (l-w) w w)) W.centeredCube
    `W.union`
    (W.translate (V3 ((l-w)/2) 0 0) . W.rotate (V3 0 0 1) pi) (knobO w)

minimalJunction :: Double -> Double -> W.Solid
minimalJunction l w = let d = 0.5*l in
    W.translate (V3 d 0 0) (junctionI l w)
    `W.union`
    W.translate (V3 (-d-w) 0 0) (junctionO l w)

main :: IO ()
main = do
    W.writeSTL 0.001 "MinimalJunction.stl" $ minimalJunction 20 5
    W.writeDiagramSVG "MinimalJunction.svg" $ W.solidDiagram (V3 1 (-1) 1) $ minimalJunction 200 50

