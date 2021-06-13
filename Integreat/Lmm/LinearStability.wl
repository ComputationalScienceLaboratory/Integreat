(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`LinearStability`"];
Integreat`Lmm`LinearStability::usage = "Package containing functions for analyzing the linear stability of linear multistep methods";

LmmLinearStabilityPolynomial::usage = "";
LmmLinearStabilityPlot::usage = "Plots the region of linear stability";
LmmOrderStarPlot::usage = "";


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
