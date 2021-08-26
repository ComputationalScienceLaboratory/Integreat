(* ::Package:: *)

BeginPackage["Integreat`Rk`Catalog`"];
Integreat`Rk`Catalog::usage = "Package containing Runge-Kutta methods from the literature";


Begin["`Private`"];
Needs["Integreat`Rk`Methods`"];
Needs["Integreat`Internal`Catalog`"];


AddCatalogSearch[Rk];

Rk[] := Rk[] = Dataset[{
	(* Explicit *)
	(* Order 1 *)
	CatalogEntry[
		{"Euler's method", "Euler method", "Forward Euler", "Explicit Euler", "FE"},
		Rk[
			{{0}},
			{\[FormalTheta]},
			{0}
		],
		"Euler's method is the simplest of all Runge\[Dash]Kutta methods.  While its stability and accuracy are limited, it is of great historical significance and serves as the foundation for many time integration methods.  Dense output is included as it is trivially available.",
		"Book",
		"Institutiones calculi integralis",
		{"Leonhard Euler"},
		1768,
		"https://archive.org/details/institutionescal020326mbp",
		"Volume" -> "1"
	],
	CatalogEntry[
		{"Coriolis' Method"},
		Rk[
			{{0, 0}, {1, 0}},
			{1/2, 1/2},
			{0, 0}
		],
		"This method is an extension of the trapezoidal rule used to approximate definite integrals.  Coriolis incorrectly thought the order was two, but the abscissae makes the method first order.",
		"Article",
		"M\[EAcute]moire sur le degr\[EAcute] d\[CloseCurlyQuote]approximation qu\[CloseCurlyQuote]on obtient pour les valeurs num\[EAcute]riques d\[CloseCurlyQuote]une variable qui satisfaita une \[EAcute]quation diff\[EAcute]rentielle, en employant pour calculer ces valeurs diverses \[EAcute]quations aux diff\[EAcute]rences plus ou moins approch\[EAcute]es.",
		{"Gaspard-Gustave de Coriolis"},
		1837,
		"https://archive.org/details/s1journaldemat02liou",
		"Journal" -> "Journal de Math\[EAcute]matiques Pures et Appliqu\[EAcute]es",
		"Volume" -> "2",
		"Pages" -> "229\[Dash]244"
	],
	CatalogEntry[
		{"Fehlberg's 1(2) method", "Runge\[Dash]Kutta\[Dash]Fehlberg 1(2)", "RKF1(2)", "RKF12"},
		Rk[
			{{0, 0, 0}, {1/2, 0, 0}, {1/256, 255/256, 0}}, {1/256, 255/256, 0},
			{0, 1/2, 1},
			{1/512, 255/256, 1/512}
		],
		"Fehlberg designed this method so that the embedded method is one order higher than the primary method.",
		"Technical Report",
		"Low-order classical Runge-Kutta formulas with stepsize control and their application to some heat transfer problems",
		{"Erwin Fehlberg"},
		1969,
		"https://archive.org/details/NASA_NTRS_Archive_19690021375",
		"Institution" -> "NASA",
		"Type" -> "NASA TR",
		"Number" -> "R-315"
	],
	(* Order 2 *)
	CatalogEntry[
		{"Explicit trapezoidal method", "Heun's method", "Improved Euler's Method", "Modified Euler's Method"},
		Rk[
			{{0, 0}, {1, 0}},
			{1/2, 1/2},
			{0, 1}
		],
		"This method is an extension of the trapezoidal rule used to approximate definite integrals.",
		"Article",
		"Ueber die numerische Aufl\[ODoubleDot]sung von Differentialgleichungen",
		{"Carl Runge"},
		1895,
		"https://doi.org/10.1007/BF01446807",
		"Journal" -> "Mathematische Annalen",
		"Volume" -> "46",
		"Number" -> "2",
		"Pages" -> "167\[Dash]178"
	],
	CatalogEntry[
		{"Explicit midpoint method", "Explicit midpoint rule"},
		Rk[
			{{0, 0}, {1/2, 0}},
			{0, 1},
			{0, 1/2}
		],
		"This method is an extension of the midpoint rule used to approximate definite integrals.",
		"Article",
		"Ueber die numerische Aufl\[ODoubleDot]sung von Differentialgleichungen",
		{"Carl Runge"},
		1895,
		"https://doi.org/10.1007/BF01446807",
		"Journal" -> "Mathematische Annalen",
		"Volume" -> "46",
		"Number" -> "2",
		"Pages" -> "167\[Dash]178"
	],
	CatalogEntry[
		{"3-stage explicit trapezoidal method"},
		Rk[
			{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}},
			{1/2, 0, 1/2},
			{0, 1, 1}
		],
		"This method is an extension of the trapezoidal rule used to approximate definite integrals.",
		"Article",
		"Ueber die numerische Aufl\[ODoubleDot]sung von Differentialgleichungen",
		{"Carl Runge"},
		1895,
		"https://doi.org/10.1007/BF01446807",
		"Journal" -> "Mathematische Annalen",
		"Volume" -> "46",
		"Number" -> "2",
		"Pages" -> "167\[Dash]178"
	],
	CatalogEntry[
		{"Ralston's method", "Ralston's second order method"},
		Rk[
			{{0, 0}, {2/3, 0}},
			{1/4, 3/4},
			{0, 2/3}
		],
		"Kopal derived the parameterized family of two-stage, second order, explicit Runge\[Dash]Kutta method and considered this method as an example. Anthony Ralston later showed that this method provides minimal error.",
		"Book",
		"Numerical Analysis",
		{"Zden\[EHacek]k Kopal"},
		1955,
		"https://archive.org/details/numericalanalysi0000kopa",
		"Publisher" -> "John Wiley & Sons",
		"Edition" -> 1
	],
	CatalogEntry[
		{"Fehlberg's 2(3) method", "Runge\[Dash]Kutta\[Dash]Fehlberg 2(3)", "RKF2(3)", "RKF23"},
		Rk[
			{{0, 0, 0}, {1, 0, 0}, {1/4, 1/4, 0}},
			{1/2, 1/2, 0},
			{0, 1, 1/2},
			{1/6, 1/6, 2/3}
		],
		"Fehlberg designed this method so that the embedded method is one order higher than the primary method.",
		"Technical Report",
		"Low-order classical Runge-Kutta formulas with stepsize control and their application to some heat transfer problems",
		{"Erwin Fehlberg"},
		1969,
		"https://archive.org/details/NASA_NTRS_Archive_19690021375",
		"Institution" -> "NASA",
		"Type" -> "NASA TR",
		"Number" -> "R-315"
	],
	CatalogEntry[
		{"Fehlberg's 2(3)b method", "Runge\[Dash]Kutta\[Dash]Fehlberg 2(3)b", "RKF2(3)b", "RKF23b"},
		Rk[
			{{0, 0, 0, 0}, {1/4, 0, 0, 0}, {-189/800, 729/800, 0, 0}, {214/891, 1/33, 650/891, 0}},
			{214/891, 1/33, 650/891, 0},
			{0, 1/4, 27/40, 1},
			{533/2106, 0, 800/1053, -1/78}
		],
		"Fehlberg designed this method so that the embedded method is one order higher than the primary method.",
		"Technical Report",
		"Low-order classical Runge-Kutta formulas with stepsize control and their application to some heat transfer problems",
		{"Erwin Fehlberg"},
		1969,
		"https://archive.org/details/NASA_NTRS_Archive_19690021375",
		"Institution" -> "NASA",
		"Type" -> "NASA TR",
		"Number" -> "R-315"
	],
	CatalogEntry[
		{"Sofroniou\[Dash]Spaletta 2(1)", "Sofroniou and Spaletta 2(1)"},
		Rk[
			{{0, 0, 0}, {1, 0, 0}, {1/2, 1/2, 0}},
			{1/2, 1/2, 0},
			{0, 1, 1},
			{1, -1/6, 1/6}
		],
		"This method is designed to have minimal error while having stiffness detection capabilities.  It is an extension of Heun's method to have the FSAL property and include an embedded method.",
		"Article",
		"Construction of Explicit Runge-Kutta Pairs with Stiffness Detection",
		{"Mark Sofroniou", "Giulia Spaletta"},
		2004,
		"https://doi.org/10.1016/j.mcm.2005.01.010",
		"Journal" -> "Mathematical and Computer Modelling",
		"Volume" -> "40",
		"Number" -> "11\[Dash]12",
		"Pages" -> "1157\[Dash]1169"
	],
	(* Order 3 *)
	CatalogEntry[
		{"Runge's third order Simpson's Rule"},
		Rk[
			{{0, 0, 0, 0}, {1/2, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}},
			{1/6, 2/3, 0, 1/6},
			{0, 1/2, 1, 1}
		],
		"This method is Runge's attempt to extend Simpson's rule for definite integrals to differential equations.  It can be expressed as M+(T'-M)/3, where M is the explicit midpoint method and T' is the 3-stage explicit trapezoidal method.",
		"Article",
		"Ueber die numerische Aufl\[ODoubleDot]sung von Differentialgleichungen",
		{"Carl Runge"},
		1895,
		"https://doi.org/10.1007/BF01446807",
		"Journal" -> "Mathematische Annalen",
		"Volume" -> "46",
		"Number" -> "2",
		"Pages" -> "167\[Dash]178"
	],
	CatalogEntry[
		{"Heun's third order method"},
		Rk[
			{{0, 0, 0}, {1/3, 0, 0}, {0, 2/3, 0}},
			{1/4, 0, 3/4},
			{0, 1/3, 2/3}
		],
		"Heun considered methods in which a stage is only used in the subsequent stage.",
		"Article",
		"Neue Methode zur approximativen Integration der Differentialgleichungen einer unabh\[ADoubleDot]ngigen Ver\[ADoubleDot]nderlichen",
		{"Karl Heun"},
		1900,
		"https://archive.org/details/zeitschriftfrma01runggoog",
		"Journal" -> "Zeitschrift f\[UDoubleDot]r Mathematik und Physik",
		"Volume" -> "45",
		"Pages" -> "23\[Dash]38"
	],
	CatalogEntry[
		{"Ralston's third order method"},
		Rk[
			{{0, 0, 0}, {1/2, 0, 0}, {0, 3/4, 0}},
			{2/9, 1/3, 4/9},
			{0, 1/2, 3/4}
		],
		"Ralson derived this method so that it has the smallest error among three-stage, third order, explicit Runge\[Dash]Kutta methods.",
		"Article",
		"Runge-Kutta Methods with Minimum Error Bounds",
		{"Anthony Ralston"},
		1962,
		"https://doi.org/10.2307/2003133",
		"Journal" -> "Mathematics of Computation",
		"Volume" -> "16",
		"Number" -> "80",
		"Pages" -> "431\[Dash]437"
	],
	CatalogEntry[
		{"Sofroniou\[Dash]Spaletta 3(2)", "Sofroniou and Spaletta 3(2)"},
		Rk[
			{{0, 0, 0, 0}, {1/2, 0, 0, 0}, {-1, 2, 0, 0}, {1/6, 2/3, 1/6, 0}},
			{1/6, 2/3, 1/6, 0},
			{0, 1/2, 1, 1},
			{(22-Sqrt[82])/72, (14+Sqrt[82])/36, (Sqrt[82]-4)/144, (16-Sqrt[82])/48}
		],
		"This method is designed to have minimal error while having stiffness detection capabilities.",
		"Article",
		"Construction of Explicit Runge-Kutta Pairs with Stiffness Detection",
		{"Mark Sofroniou", "Giulia Spaletta"},
		2004,
		"https://doi.org/10.1016/j.mcm.2005.01.010",
		"Journal" -> "Mathematical and Computer Modelling",
		"Volume" -> "40",
		"Number" -> "11\[Dash]12",
		"Pages" -> "1157\[Dash]1169"
	],
	CatalogEntry[
		{"Bogacki\[Dash]Shampine method", "Bogacki\[Dash]Shampine 3(2)", "BS(2,3)", "ode23"},
		Rk[
			{{0, 0, 0, 0}, {1/2, 0, 0, 0}, {0, 3/4, 0, 0}, {2/9, 1/3, 4/9, 0}},
			{2/9, 1/3, 4/9, 0},
			{0, 1/2, 3/4, 1},
			{7/24, 1/4, 1/3, 1/8}
		],
		"This extends Ralston's optimal third order method by adding an embedded method and the FSAL property.  It is perhaps best known from MATLAB's ode23 function.",
		"Acticle",
		"A 3(2) pair of Runge - Kutta formulas",
		{"Lawrence F. Shampine", "Przemyslaw Bogacki"},
		1989,
		"https://doi.org/10.1016/0893-9659(89)90079-7",
		"Journal" -> "Applied Mathematics Letters",
		"Volume" -> "2",
		"Number" -> "4",
		"Pages" -> "321\[Dash]325"
	],
	(* Order 4 *)
	CatalogEntry[
		{"RK4", "Classic Runge\[Dash]Kutta Method", "Classical Runge\[Dash]Kutta Method", "The Runge\[Dash]Kutta Method"},
		Rk[
			{{0, 0, 0, 0}, {1/2, 0, 0, 0}, {0, 1/2, 0, 0}, {0, 0, 1, 0}},
			{1/6, 1/3, 1/3, 1/6},
			{0, 1/2, 1/2, 1}
		],
		"RK4 is the most well-known Runge\[Dash]Kutta method.  It is an extension of Simpson's rule used to approximate definite integrals.",
		"Article",
		"Beitrag zur na\:0308herungsweisen Integration totaler Differentialgleichungen",
		{"Wilhelm Kutta"},
		1901,
		"https://archive.org/details/zeitschriftfrma12runggoog",
		"Journal" -> "Zeitschrift f\[UDoubleDot]r Mathematik und Physik",
		"Volume" -> "46",
		"Pages" -> "435\[Dash]453"
	],
	CatalogEntry[
		{"3/8-rule"},
		Rk[
			{{0, 0, 0, 0}, {1/3, 0, 0, 0}, {-1/3, 1, 0, 0}, {1, -1, 1, 0}},
			{1/8, 3/8, 3/8, 1/8},
			{0, 1/3, 2/3, 1}
		],
		"This method is an extension of the Simpson's 3/8 rule used to approximate definite integrals.",
		"Article",
		"Beitrag zur na\:0308herungsweisen Integration totaler Differentialgleichungen",
		{"Wilhelm Kutta"},
		1901,
		"https://archive.org/details/zeitschriftfrma12runggoog",
		"Journal" -> "Zeitschrift f\[UDoubleDot]r Mathematik und Physik",
		"Volume" -> "46",
		"Pages" -> "435\[Dash]453"
	],
	CatalogEntry[
		{"Gill's Method", "Gill's fourth order method", "Runge\[Dash]Kutta\[Dash]Gill method"},
		Rk[
			{{0, 0, 0, 0}, {1/2, 0, 0, 0}, {(Sqrt[2]-1)/2, 1-Sqrt[2]/2, 0, 0}, {0, -Sqrt[2]/2, 1+Sqrt[2]/2, 0}},
			{1/6, (2-Sqrt[2])/6, (2+Sqrt[2])/6, 1/6},
			{0, 1/2, 1/2, 1}
		],
		"Using Wilhelm Kutta's parameterization of fourth order Rugne\[Dash]Kutta methods, Gill searched for a method that can use three registers per variable instead of four.  Of the two he found, this is the more accurate.",
		"Article",
		"A process for the step-by-step integration of differential equations in an automatic digital computing machine",
		{"Stanley Gill"},
		1951,
		"https://doi.org/10.1017/S0305004100026414",
		"Journal" -> "Mathematical Proceedings of the Cambridge Philosophical Society",
		"Volume" -> "47",
		"Number" -> "1",
		"Pages" -> "96\[Dash]108"
	],
	CatalogEntry[
		{"Merson's method", "Merson 4(\"5\")", "Runge\[Dash]Kutta\[Dash]Merson method"},
		Rk[
			{{0, 0, 0, 0, 0}, {1/3, 0, 0, 0, 0}, {1/6, 1/6, 0, 0, 0}, {1/8, 0, 3/8, 0, 0}, {1/2, 0, -3/2, 2, 0}},
			{1/6, 0, 0, 2/3, 1/6},
			{0, 1/3, 1/3, 1/2, 1},
			{1/10, 0, 3/10, 2/5, 1/5}
		],
		"The embedded method is order three for general, nonlinear problems but order five for linear problems.",
		"Proceedings",
		"An operational method for the study of integration processes",
		{"Robert H. \"Robin\" Merson"},
		1957,
		"https://www.massey.ac.nz/~rmclachl/DPACM", (* Is there a more stable url? *)
		"Book Title" -> "Data processing & automatic computing machines",
		"Volume" -> "1",
		"Pages" -> "110-1 to 110-26"
	],
	CatalogEntry[
		{"Sofroniou\[Dash]Spaletta 4(3)", "Sofroniou and Spaletta 4(3)"},
		Rk[
			{{0, 0, 0, 0, 0}, {2/5, 0, 0, 0, 0}, {-3/20, 3/4, 0, 0, 0}, {19/44, -15/44, 10/11, 0, 0}, {11/72, 25/72, 25/72, 11/72, 0}},
			{11/72, 25/72, 25/72, 11/72, 0},
			{0, 2/5, 3/5, 1, 1},
			{1251515/8970912, 3710105/8970912, 2519695/8970912, 61105/8970912, 119041/747576}
		],
		"This method is designed to have stiffness detection capabilities, simple coefficients, and minimal error.",
		"Article",
		"Construction of Explicit Runge-Kutta Pairs with Stiffness Detection",
		{"Mark Sofroniou", "Giulia Spaletta"},
		2004,
		"https://doi.org/10.1016/j.mcm.2005.01.010",
		"Journal" -> "Mathematical and Computer Modelling",
		"Volume" -> "40",
		"Number" -> "11\[Dash]12",
		"Pages" -> "1157\[Dash]1169"
	]
}];


End[];
EndPackage[];
