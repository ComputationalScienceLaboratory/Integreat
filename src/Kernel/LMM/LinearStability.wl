(* ::Package:: *)

(* ::Section:: *)
(*Package Definitions*)


LMMLinearStabilityPolynomial[lmm_LMM, zeta_, mu_] := LMMAlphaGeneratingPolynomial[lmm, zeta] - mu * LMMBetaGeneratingPolynomial[lmm, zeta];


(* TODO: remove dependence on GLM plotting *)
LMMLinearStabilityPlot[
	lmm_LMM,
	bounds:(PatternSequence[] | _?NumericQ | {_?NumericQ, _?NumericQ}),
	opts:OptionsPattern[ComplexRegionPlot]
] := GLMLinearStabilityPlot[GLM[lmm], bounds, opts];


LMMOrderStarPlot[
	lmm_LMM,
	bounds:(PatternSequence[] | _?NumericQ | {_?NumericQ, _?NumericQ}),
	opts:OptionsPattern[ComplexRegionPlot]
] := GLMOrderStarPlot[GLM[lmm], bounds, opts];
