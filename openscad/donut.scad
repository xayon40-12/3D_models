module donut(inner_radius = 10, outer_radius = 5) {
	rotate_extrude(angle = 360, convexity = 10) 
		translate([inner_radius, 0, 0])
		circle(outer_radius);
}
