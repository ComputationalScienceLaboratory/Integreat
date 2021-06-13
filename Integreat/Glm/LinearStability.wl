(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Glm`LinearStability`"];
Integreat`Glm`LinearStability::usage = "Package containing functions for analyzing the linear stability of general linear methods";

GlmLinearStability::usage = "The linear stability matrix for a general linear method";
GlmLinearStabilityPolynomial::usage = "The linear stability function for a general linear method";
GlmLinearStabilityPlot::usage = "Plots the region of linear stability";
GlmOrderStarPlot::usage = "";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`Glm`Methods`", "Integreat`Internal`LinearStability`"}];


(* ::Section:: *)
(*Package Definitions*)


GlmLinearStability[glm_Glm, lim_DirectedInfinity] := Limit[GlmLinearStability[glm, z], z -> lim];
GlmLinearStability[glm_Glm, z_] := GlmV[glm] + z * GlmB[glm] . Inverse[IdentityMatrix[GlmInternalStages[glm]] - z * GlmA[glm]] . GlmU[glm];

GlmLinearStabilityPolynomial[glm_Glm, w_, z_] := Det[w * IdentityMatrix[GlmExternalStages[glm]] - GlmLinearStability[glm, z]];

GlmLinearStabilityPlot[glm_Glm, bounds:Repeated[{_, _}, {0, 2}], opts:OptionsPattern[RegionPlot]] := LinearStabilityPlot[
	Evaluate[Norm[Eigenvalues[GlmLinearStability[glm, #]], Infinity]] &, bounds, opts
];

GlmOrderStarPlot[glm_Glm, bounds:Repeated[{_, _}, {0, 2}], opts:OptionsPattern[RegionPlot]] := OrderStarPlot[
	Evaluate[Norm[Eigenvalues[GlmLinearStability[glm, #]], Infinity]] &, bounds, opts
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
