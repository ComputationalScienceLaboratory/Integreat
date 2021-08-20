(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`LMM`LinearStability`"];
Integreat`LMM`LinearStability::usage = "This package contains functions for analyzing the stability of linear multistep methods applied to the linear test problem y' = \[Lambda] y.";

LMMLinearStabilityPolynomial::usage = "LMMLinearStabilityPolynomial[lmm, \[Zeta], \[Mu]] creates a polynomial in \[Zeta], parameterized by \[Mu] = h \[Lambda], that determines the linear stability of lmm.";
LMMLinearStabilityPlot::usage =
	"LMMLinearStabilityPlot[lmm] plots the linear stability region of lmm.\n" <>
	"LMMLinearStabilityPlot[lmm, {xMin, xMax}, {yMin, yMax}] plots within the specified bounds.\n" <>
	"LMMLinearStabilityPlot[\[Ellipsis], opts] plots with RegionPlot options opts.";
LMMOrderStarPlot::usage =
	"LMMOrderStarPlot[lmm] plots the order star of lmm.\n" <>
	"LMMOrderStarPlot[lmm, {xMin, xMax}, {yMin, yMax}] plots within the specified bounds.\n" <>
	"LMMOrderStarPlot[\[Ellipsis], opts] plots with RegionPlot options opts.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {
	"Integreat`LMM`Methods`",
	"Integreat`GLM`Methods`",
	"Integreat`GLM`LinearStability`"
}];


(* ::Section:: *)
(*Package Definitions*)


LMMLinearStabilityPolynomial[lmm_LMM, zeta_, mu_] := LMMAlphaGeneratingPolynomial[lmm, zeta] - mu * LMMBetaGeneratingPolynomial[lmm, zeta];

LMMLinearStabilityPlot[lmm_LMM, bounds:Repeated[{_, _}, {0, 2}], opts:OptionsPattern[RegionPlot]] := GLMLinearStabilityPlot[GLM[lmm], bounds, opts];

LMMOrderStarPlot[lmm_LMM, bounds:Repeated[{_, _}, {0, 2}], opts:OptionsPattern[RegionPlot]] := GLMOrderStarPlot[GLM[lmm], bounds, opts];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
