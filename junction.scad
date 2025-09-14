junction_test($fa = 1, $fs = 0.4);

module junction_test(l = 20, w = 4) {
	translate([2*w, 0, 0]) junction_r(l, w);
	translate([0, 0*4*w, 0]) junction_l(l, w, 0);
	translate([0, 1*4*w, 0]) with_n(1, l, w) junction_l(l, w, 0.25);
	translate([0, 2*4*w, 0]) with_n(2, l, w) junction_l(l, w, 0.5);
	translate([0, 3*4*w, 0]) with_n(3, l, w) junction_l(l, w, 0.75);
}

module with_n(n, l, w) {
	difference() {
		children(0);
		let(e = 0.001, d = 0.5*l/n)
		for (i = [0:n-1]) translate([-l+0.25*d+i*d, 0, 0.5*1.5*w-0.1*w]) cube([0.5*d+e, w+e, 0.2*w+e], center = true);
	}
}

module junction_l(l, w, enclose = 0.5, tolerance = -0.2, thickness = 0.5) {
  let (e = 0.001, to = tolerance, th = w*thickness)
	union() {
		let (l = l-(w+to)) translate([-l/2-(w+to), 0, 0]) cube([l, w, 1.5*w], center = true);
		difference() {
			intersection() {
				cube([3*w, 3*w, 1.5*w], center = true);
				sphere(w+th);
			}
			sphere(w+to);
			translate([(0.6+0.4*enclose)*sqrt(2)*w-tolerance, 0, 0]) rotate([0, 0, 45]) cube(2*w, center = true);
		}
	};
}

module junction_r(l, w) {
	let (e = 0.001)
	union() {
		translate([l/2, 0, 0]) cube([l, w, 1.5*w], center = true);
		translate([0, 0, 0])
		intersection() {
			cube([2*w, 2*w, 1.5*w], center = true);
			sphere(w);
		}
	}
}
