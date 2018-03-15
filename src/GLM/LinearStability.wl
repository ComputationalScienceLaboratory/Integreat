(* ::Package:: *)

BeginPackage["CSL`OdeUtils`RungeKutta`LinearStability`"];


CSL`OdeUtils`RungeKutta`LinearStability::usage = "Package containing functions for analyzing the linear stability Runge-Kutta methods";

RungeKuttaInternalLinearStability::usage = "The linear stability function for the stages of a Runge-Kutta method";
RungeKuttaLinearStability::usage = "The linear stability function for a Runge-Kutta method";
RungeKuttaLinearStabilityPlot::usage = "Plots the region of linear stability";
RungeKuttaLinearStabilityP::usage = "The numerator of the linear stability function";
RungeKuttaLinearStabilityQ::usage = "The denominator of the linear stability function";
RungeKuttaLinearStabilityE::usage = "The E-polynomial to test for I-stability";
RungeKuttaAStableQ::usage = "Returns True if the Runge-Kutta method is A-stable, and False otherwise";
RungeKuttaStifflyAccurateQ::usage = "Determines if a Runge-Kutta method is stiffly-accurate";


Begin["`Private`"];
Needs["CSL`OdeUtils`RungeKutta`Methods`"];


RungeKuttaInternalLinearStability[method_/;RungeKuttaQ[method], z_] := With[{
	s = RungeKuttaStages[method]
},
	LinearSolve[IdentityMatrix[s] - z * method[\[FormalCapitalA]], ConstantArray[1, s]]
];

RungeKuttaLinearStability[method_/;RungeKuttaQ[method], z_] := 1 + z * method[\[FormalB]].RungeKuttaInternalLinearStability[method, z];

SetAttributes[RungeKuttaLinearStabilityPlot, HoldAll];
RungeKuttaLinearStabilityPlot[method_/;RungeKuttaQ[method], {xMin_, xMax_}, {yMin_, yMax_}, opts:OptionsPattern[RegionPlot]] := RegionPlot[
	Abs[RungeKuttaLinearStability[method, realPart + imagPart * I]] <= 1,
	{realPart, xMin, xMax},
	{imagPart, yMin, yMax},
	opts,
	FrameLabel -> {"Re", "Im"}
];
RungeKuttaLinearStabilityPlot[method_/;RungeKuttaQ[method], opts:OptionsPattern[RegionPlot]] := RungeKuttaLinearStabilityPlot[method, {-6, 2}, {-4, 4}, opts];

RungeKuttaLinearStabilityP[method_/;RungeKuttaQ[method], z_] := With[{
	s = RungeKuttaStages[method]
},
	Det[IdentityMatrix[s] + z * (ConstantArray[Flatten[method[\[FormalB]]], s] - method[\[FormalCapitalA]])]
];

RungeKuttaLinearStabilityQ[method_/;RungeKuttaQ[method], z_] := Det[IdentityMatrix[RungeKuttaStages[method]] - z * method[\[FormalCapitalA]]];
RungeKuttaLinearStabilityE[method_/;RungeKuttaQ[method], y_] :=
	RungeKuttaLinearStabilityQ[method, y * I] * RungeKuttaLinearStabilityQ[method, -y * I] -
	RungeKuttaLinearStabilityP[method, y * I] * RungeKuttaLinearStabilityP[method, -y * I];

RungeKuttaAStableQ[method_/;RungeKuttaQ[method]] := Resolve[ForAll[y, ComplexExpand[RungeKuttaLinearStabilityE[method, y]] >= 0], Reals]

RungeKuttaStifflyAccurateQ[method_/;RungeKuttaQ[method]] := And @@ Thread[Last[method[\[FormalCapitalA]]] == method[\[FormalB]]];


End[];


EndPackage[];
