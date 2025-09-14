module spoked_wheel(inner_radius = 10, outer_radius = 12, width = 2, spokes = 5) {
	let (ir = inner_radius, or = outer_radius, w = width, n = spokes, e = 0.001) { 
		for (i = [0:n-1])
			rotate([0, 0, i*360/n])
			rotate([0, 90, 0])
			cylinder(h = ir, r = w/2, center = false);

			difference() {
				cylinder(h = w, r = or, center = true);
				cylinder(h = w+e, r = ir, center = true);
			}
	}
}
