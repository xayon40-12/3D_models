E = 0.001;

test($fa = 1, $fs = 0.4);

module with_n(n, l, w) {
	difference() {
		children(0);
		let(d = 0.25*l/n)
		for (i = [0:n-1]) translate([-0.126*l+i*d, 0, w/2-0.05*w]) tol() cube([0.02*l, 1000, 0.1*w], center = true);
	}
}

module test(l = 20, w = 5) {
	test_both(l, w, 0);
	translate([0, 4*w, 0]) with_n(1, l, w) test_both(l, w, -0.1);
	translate([0, 8*w, 0]) with_n(2, l, w) test_both(l, w, -0.2);
}

module test_both(l = 20, w = 5, tolerance = -0.2) {
	junction(l, w, tolerance);
	translate([0, 2*w, 0]) junction_90(l, w, tolerance);
}

module junction(l, w, tolerance) {
	translate([-l/4, 0, 0]) junction_o(l/2, w, tolerance);
	translate([l/4-E, 0, 0]) rotate([0, 0, 180]) junction_i(l/2, w);
}

module junction_90(l, w, tolerance) {
	translate([-l/4, 0, 0]) junction_o(l/2, w, tolerance);
	translate([l/4-E, 0, 0]) rotate([90, 0, 180]) junction_i(l/2, w);
}

module knob(w, tolerance = 0) {
	let (t = tolerance) {
		translate([0, 0, w/4+t]) cylinder(w/4, w/2, w/8);
		translate([0, 0, -w/4-E-t]) cylinder(w/2+2*(t+E), w/2, w/2);
		translate([0, 0, -w/2-t]) cylinder(w/4, w/8, w/2);
	}
}

module junction_o(l, w, tolerance) {
	difference() {
		union() {
			cube([l, w, w], center = true);
			translate([(-l+w)/2+0.15*w, 0, 0]) cube([1.3*w, 1.3*w, w], center = true);
		}
		translate([(-l+w)/2, 0, 0]) tol() cube([w, w/2+w/16, w], center=true);
		translate([(-l+w)/2, 0, 0]) rotate([-90, 0, 0]) tol() knob(w, tolerance);
		difference() {
			translate([-l/2+w/4, 0, 0]) tol() cube([w/2, 2*w, w], center=true);
			translate([(-l+w)/2, 0, 0]) rotate([-90, 0, 0]) cylinder(h = 2*w, r = w/2, center = true);
		}
	}
}

module junction_i(l, w) {
	difference() {
		cube([l, w, w], center = true);
		translate([(-l+w)/2, 0, 0]) tol() cube([w, w, w], center=true);
	}
	translate([-l/2+w*3/4, 0, 0]) tol() cube([w/2, w/2, w], center=true);
	translate([(-l+w)/2, 0, 0]) rotate([-90, 0, 0]) tol() knob(w);
}

module tol() {
	scale(1+E) children(0);
}
