(* ::Package:: *)

BeginPackage["Integreat`GLM`LinearStability`", {"Integreat`GLM`Methods`", "Integreat`Internal`LinearStability`"}];


Integreat`GLM`LinearStability::usage = "Package containing functions for analyzing the linear stability of general linear methods";

GlmLinearStabilityMatrix::usage = "The linear stability matrix for a general linear method";
GlmLinearStabilityFunction::usage = "The linear stability function for a general linear method";
GlmLinearStabilityPlot::usage = "Plots the region of linear stability";


Begin["`Private`"];


GlmLinearStabilityMatrix[glm_Glm, z_] := GlmV[glm] + z * GlmB[glm] . Inverse[IdentityMatrix[GlmInternalStages[glm]] - z * GlmA[glm]] . GlmU[glm];

GlmLinearStabilityFunction[glm_Glm, w_, z_] := Det[w * IdentityMatrix[GlmExternalStages[glm]] - GlmLinearStabilityMatrix[glm, z]];

GlmLinearStabilityPlot[glm_Glm, args___] := LinearStabilityPlot[Evaluate[Norm[Eigenvalues[GlmLinearStabilityMatrix[glm, #]], Infinity]] &, args];


End[];
EndPackage[];
