(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`LMM`LinearStability`"];

LMMLinearStabilityPolynomial::usage = "LMMLinearStabilityPolynomial[lmm, \[Zeta], \[Mu]] creates a polynomial in \[Zeta], parameterized by \[Mu]=h*\[Lambda], that determines the linear stability of lmm.";
LMMLinearStabilityPlot::usage =
	"LMMLinearStabilityPlot[lmm] plots the linear stability region of lmm.\n" <>
	"LMMLinearStabilityPlot[lmm, z] plots in the square region -z-z*I to z+z*I.\n" <>
	"LMMLinearStabilityPlot[lmm, {zMin, zMax}] plots in a region with corners specified by the complex numbers zMin and zMax.";
LMMOrderStarPlot::usage =
	"LMMOrderStarPlot[lmm] plots the order star of lmm.\n" <>
	"LMMOrderStarPlot[lmm, z] plots in the square region -z-z*I to z+z*I.\n" <>
	"LMMOrderStarPlot[lmm, {zMin, zMax}] plots in a region with corners specified by the complex numbers zMin and zMax.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {
	"Integreat`LMM`Core`",
	"Integreat`GLM`Core`",
	"Integreat`GLM`LinearStability`"
}];


(* ::Section:: *)
(*Package Definitions*)


LMMLinearStabilityPolynomial[lmm_LMM, zeta_, mu_] := LMMAlphaGeneratingPolynomial[lmm, zeta] - mu * LMMBetaGeneratingPolynomial[lmm, zeta];

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


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
