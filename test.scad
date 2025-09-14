use <spoked_wheel.scad>
use <donut.scad>
use <foil.scad>
use <junction.scad>

$fa = 1;
$fs = 0.4;

translate([30, 0, 0]) spoked_wheel();
translate([0, 30, 0]) donut();
translate([-30, 0, 0]) foil(10, 10);
junction();
