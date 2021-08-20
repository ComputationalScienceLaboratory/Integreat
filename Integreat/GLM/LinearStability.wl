(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`GLM`LinearStability`"];
Integreat`GLM`LinearStability::usage = "Package containing functions for analyzing the linear stability of general linear methods";

GLMLinearStability::usage = "The linear stability matrix for a general linear method";
GLMLinearStabilityPolynomial::usage = "The linear stability function for a general linear method";
GLMLinearStabilityPlot::usage = "Plots the region of linear stability";
GLMOrderStarPlot::usage = "";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`GLM`Methods`", "Integreat`Internal`LinearStability`"}];


(* ::Section:: *)
(*Package Definitions*)


GLMLinearStability[glm_GLM, lim_DirectedInfinity] := Limit[GLMLinearStability[glm, z], z -> lim];
GLMLinearStability[glm_GLM, z_] := GLMV[glm] + z * GLMB[glm] . Inverse[IdentityMatrix[GLMInternalStages[glm]] - z * GLMA[glm]] . GLMU[glm];

GLMLinearStabilityPolynomial[glm_GLM, w_, z_] := Det[w * IdentityMatrix[GLMExternalStages[glm]] - GLMLinearStability[glm, z]];

GLMLinearStabilityPlot[glm_GLM, bounds:Repeated[{_, _}, {0, 2}], opts:OptionsPattern[RegionPlot]] := LinearStabilityPlot[
	Evaluate[Norm[Eigenvalues[GLMLinearStability[glm, #]], Infinity]] &, bounds, opts
];

GLMOrderStarPlot[glm_GLM, bounds:Repeated[{_, _}, {0, 2}], opts:OptionsPattern[RegionPlot]] := OrderStarPlot[
	Evaluate[Norm[Eigenvalues[GLMLinearStability[glm, #]], Infinity]] &, bounds, opts
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
