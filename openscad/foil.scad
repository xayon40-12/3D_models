function foil_thickness(t, x) = 5*t*(0.2969*sqrt(x) - 0.1281*x - 0.3516*x^2 + 0.2843*x^3 - 0.1015*x^4);

module foil(w, l, t = 0.2, n = 100) {
	translate([0, 0, -w/2])
	linear_extrude(w)
	scale(l)
	polygon([
		for (x = [0:1/n:1]) [x, foil_thickness(t, x)],
		for (x = [1/n:1/n:1-1/n]) [1-x, -foil_thickness(t, 1-x)]
	]);
}
