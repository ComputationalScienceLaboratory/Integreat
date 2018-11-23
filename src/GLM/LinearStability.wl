(* ::Package:: *)

BeginPackage["CSL`OdeUtils`GLM`LinearStability`"];


CSL`OdeUtils`GLM`LinearStability::usage = "Package containing functions for analyzing the linear stability of general linear methods";

GlmLinearStabilityMatrix::usage = "The linear stability matrix for a general linear method";
GlmLinearStabilityFunction::usage = "The linear stability function for a general linear method";
GlmLinearStabilityPlot::usage = "Plots the region of linear stability";


Begin["`Private`"];
Needs["CSL`OdeUtils`GLM`Methods`"];


GlmLinearStabilityMatrix[method_?GlmQ, z_] := method[\[FormalCapitalV]] + z * method[\[FormalCapitalB]] . Inverse[IdentityMatrix[GlmInternalStages[method]] - z * method[\[FormalCapitalA]]] . method[\[FormalCapitalU]];

GlmLinearStabilityFunction[method_?GlmQ, w_, z_] := Det[w * IdentityMatrix[GlmExternalStages[method]] - GlmLinearStabilityMatrix[method, z]];

GlmLinearStabilityPlot[method_?GlmQ, {xMin_, xMax_}, {yMin_, yMax_}, opts:OptionsPattern[RegionPlot]] := RegionPlot[
	Abs[First[Eigenvalues[GlmLinearStabilityMatrix[method, realPart + imagPart * I], 1]]] < 1,
	{realPart, xMin, xMax},
	{imagPart, yMin, yMax},
	opts,
	FrameLabel -> {"Re", "Im"}
];
GlmLinearStabilityPlot[method_?GlmQ, opts:OptionsPattern[RegionPlot]] := GlmLinearStabilityPlot[method, {-6, 2}, {-4, 4}, opts];


End[];


EndPackage[];
