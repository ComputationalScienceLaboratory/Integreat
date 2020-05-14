(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Glm`LinearStability`"];
Integreat`Glm`LinearStability::usage = "Package containing functions for analyzing the linear stability of general linear methods";

GlmLinearStabilityMatrix::usage = "The linear stability matrix for a general linear method";
GlmLinearStabilityFunction::usage = "The linear stability function for a general linear method";
GlmLinearStabilityPlot::usage = "Plots the region of linear stability";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`Glm`Methods`", "Integreat`Internal`LinearStability`"}];


(* ::Section:: *)
(*Package Definitions*)


GlmLinearStabilityMatrix[glm_Glm, z_] := GlmV[glm] + z * GlmB[glm] . Inverse[IdentityMatrix[GlmInternalStages[glm]] - z * GlmA[glm]] . GlmU[glm];

GlmLinearStabilityFunction[glm_Glm, w_, z_] := Det[w * IdentityMatrix[GlmExternalStages[glm]] - GlmLinearStabilityMatrix[glm, z]];

GlmLinearStabilityPlot[glm_Glm, args___] := LinearStabilityPlot[Evaluate[Norm[Eigenvalues[GlmLinearStabilityMatrix[glm, #]], Infinity]] &, args];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
