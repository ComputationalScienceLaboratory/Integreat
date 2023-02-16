(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


lmmStabilityPlot[lmm_, comp_, opts___] := With[{
		stab = comp[
			Norm[
				(* Non-Root[...] solutions cause division by 0 and other numeric issues *)
				SolveValues[LMMLinearStabilityPolynomial[lmm, w, z] == 0, w, Cubics -> False, Quartics -> False],
				Infinity
			]
		]
	},
	ComplexRegionPlot[stab, opts, FrameLabel -> {"Re", "Im"}]
];


(* ::Section:: *)
(*Package Definitions*)


LMMLinearStabilityPolynomial[lmm_LMM, zeta_, mu_] := LMMAlphaGeneratingPolynomial[lmm, zeta] - mu * LMMBetaGeneratingPolynomial[lmm, zeta];


LMMLinearStabilityPlot[
	lmm_LMM,
	Optional[{zMin_?NumericQ, zMax_?NumericQ} | zMin_?NumericQ, {-6 - 4I, 2 + 4I}],
	opts:OptionsPattern[ComplexRegionPlot]
] := lmmStabilityPlot[lmm, LessEqualThan[1], {z, zMin, zMax}, opts];


LMMOrderStarPlot[
	lmm_LMM,
	Optional[{zMin_?NumericQ, zMax_?NumericQ} | zMin_?NumericQ, 4],
	opts:OptionsPattern[ComplexRegionPlot]
] := lmmStabilityPlot[lmm, GreaterThan[Exp[Re[z]]], {z, zMin, zMax}, opts];
