(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`LinearStability`"];
Integreat`Lmm`LinearStability::usage = "This package contains functions for analyzing the stability of linear multistep methods applied to the linear test problem y' = \[Lambda] y.";

LmmLinearStabilityPolynomial::usage = "LmmLinearStabilityPolynomial[lmm, \[Zeta], \[Mu]] creates a polynomial in \[Zeta], parameterized by \[Mu] = h \[Lambda], that determines the linear stability of lmm.";
LmmLinearStabilityPlot::usage =
	"LmmLinearStabilityPlot[lmm] plots the linear stability region of lmm.\n" <>
	"LmmLinearStabilityPlot[lmm, {xMin, xMax}, {yMin, yMax}] plots within the specified bounds.\n" <>
	"LmmLinearStabilityPlot[\[Ellipsis], opts] plots with RegionPlot options opts.";
LmmOrderStarPlot::usage =
	"LmmOrderStarPlot[lmm] plots the order star of lmm.\n" <>
	"LmmOrderStarPlot[lmm, {xMin, xMax}, {yMin, yMax}] plots within the specified bounds.\n" <>
	"LmmOrderStarPlot[\[Ellipsis], opts] plots with RegionPlot options opts.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {
	"Integreat`Lmm`Methods`",
	"Integreat`Glm`Methods`",
	"Integreat`Glm`LinearStability`"
}];


(* ::Section:: *)
(*Package Definitions*)


LmmLinearStabilityPolynomial[lmm_Lmm, zeta_, mu_] := LmmAlphaGeneratingPolynomial[lmm, zeta] - mu * LmmBetaGeneratingPolynomial[lmm, zeta];

LmmLinearStabilityPlot[lmm_Lmm, bounds:Repeated[{_, _}, {0, 2}], opts:OptionsPattern[RegionPlot]] := GlmLinearStabilityPlot[Glm[lmm], bounds, opts];

LmmOrderStarPlot[lmm_Lmm, bounds:Repeated[{_, _}, {0, 2}], opts:OptionsPattern[RegionPlot]] := GlmOrderStarPlot[Glm[lmm], bounds, opts];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
