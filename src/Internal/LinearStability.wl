(* ::Package:: *)

BeginPackage["CSL`OdeUtils`Internal`LinearStability`"];


CSL`OdeUtils`Internal`LinearStability::usage = "An internal package for analyzing linear stability";

LinearStabilityPlot::usage = "Plots a stability region on the complex plane";


Begin["`Private`"];


LinearStabilityPlot[stab_, {xMin_, xMax_}, {yMin_, yMax_}, opts:OptionsPattern[RegionPlot]] := RegionPlot[
	stab[a + b * I] < 1,
	{a, xMin, xMax},
	{b, yMin, yMax},
	opts,
	FrameLabel -> {"Re", "Im"}
];
LinearStabilityPlot[stab_, opts:OptionsPattern[RegionPlot]] := LinearStabilityPlot[stab, {-6, 2}, {-4, 4}, opts];
LinearStabilityPlot[___] := $Failed;


End[];


EndPackage[];
