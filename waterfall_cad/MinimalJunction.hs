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
import Control.Lens


-- module knob(w, tolerance = 0) {
-- 	let (t = tolerance) {
-- 		translate([0, 0, w/4+t]) cylinder(w/4, w/2, w/8);
-- 		translate([0, 0, -w/4-E-t]) cylinder(w/2+2*(t+E), w/2, w/2);
-- 		translate([0, 0, -w/2-t]) cylinder(w/4, w/8, w/2);
-- 	}
-- }

-- module junction_o(l, w, tolerance) {
-- 	difference() {
-- 		union() {
-- 			cube([l, w, w], center = true);
-- 			translate([(-l+w)/2+0.15*w, 0, 0]) cube([1.3*w, 1.3*w, w], center = true);
-- 		}
-- 		translate([(-l+w)/2, 0, 0]) tol() cube([w, w/2+w/16, w], center=true);
-- 		translate([(-l+w)/2, 0, 0]) rotate([-90, 0, 0]) tol() knob(w, tolerance);
-- 		difference() {
-- 			translate([-l/2+w/4, 0, 0]) tol() cube([w/2, 2*w, w], center=true);
-- 			translate([(-l+w)/2, 0, 0]) rotate([-90, 0, 0]) cylinder(h = 2*w, r = w/2, center = true);
-- 		}
-- 	}
-- }

-- module junction_i(l, w) {
-- 	difference() {
-- 		cube([l, w, w], center = true);
-- 		translate([(-l+w)/2, 0, 0]) tol() cube([w, w, w], center=true);
-- 	}
-- 	translate([-l/2+w*3/4, 0, 0]) tol() cube([w/2, w/2, w], center=true);
-- 	translate([(-l+w)/2, 0, 0]) rotate([-90, 0, 0]) tol() knob(w);
-- }

knob :: Double -> W.Solid
knob w = W.uScale w . W.rotate (V3 1 0 0) (pi/2) . W.translate (V3 0 0 (-0.375)) $
    (W.translate (V3 0 0 0) . W.uScale 0.5) W.unitCone
    `W.union`
    (W.translate (V3 0 0 0.25) . W.scale (V3 0.5 0.5 0.25)) W.unitCylinder
    `W.union`
    (W.translate (V3 0 0 0.75) . W.rotate (V3 1 0 0) pi . W.uScale 0.5) W.unitCone
    `W.union`
    (W.translate (V3 (-0) (-0.5) 0.25) . W.scale (V3 0.5 1 0.25)) W.unitCube

junctionI :: Double -> Double -> W.Solid
junctionI l w =
    (W.translate (V3 (l/2-w) 0 0) . W.scale (V3 (l-w) w w)) W.centeredCube
    `W.union`
    (W.translate (V3 (-l/2+w) 0 0)) (knob w)

minimalJunction :: Double -> Double -> W.Solid
minimalJunction l w = junctionI l w

main :: IO ()
main = do
    W.writeSTL 0.001 "MinimalJunction.stl" $ minimalJunction 20 5
    W.writeDiagramSVG "MinimalJunction.svg" $ W.solidDiagram (V3 0 0 (-1)) $ minimalJunction 200 50

