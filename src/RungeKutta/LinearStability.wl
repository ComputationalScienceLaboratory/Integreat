(* ::Package:: *)

BeginPackage["CSL`OdeUtils`RungeKutta`LinearStability`"];


CSL`OdeUtils`RungeKutta`LinearStability::usage = "Package containing functions for analyzing the linear stability of Runge-Kutta methods";

RungeKuttaLinearStabilityInternal::usage = "The linear stability function for the stages of a Runge-Kutta method";
RungeKuttaLinearStability::usage = "The linear stability function for a Runge-Kutta method";
RungeKuttaLinearStabilityPlot::usage = "Plots the region of linear stability";
RungeKuttaLinearStabilityP::usage = "The numerator of the linear stability function";
RungeKuttaLinearStabilityQ::usage = "The denominator of the linear stability function";
RungeKuttaLinearStabilityE::usage = "The E-polynomial to test for I-stability";
RungeKuttaAStableCondition::usage = "Returns True if the Runge-Kutta method is A-stable, and False otherwise";
RungeKuttaStifflyAccurateCondition::usage = "Determines if a Runge-Kutta method is stiffly-accurate";


Begin["`Private`"];
Needs["CSL`OdeUtils`RungeKutta`Methods`"];
Needs["CSL`OdeUtils`Internal`LinearStability`"];


RungeKuttaLinearStabilityInternal[rk_RungeKutta, z_] := With[{
		s = RungeKuttaStages[rk]
	},
	LinearSolve[IdentityMatrix[s] - z * RungeKuttaA[rk], ConstantArray[1, s]]
];

RungeKuttaLinearStability[rk_RungeKutta, z_] := 1 + z * RungeKuttaB[rk].RungeKuttaLinearStabilityInternal[rk, z];

RungeKuttaLinearStabilityPlot[rk_RungeKutta, args___] := LinearStabilityPlot[Abs[RungeKuttaLinearStability[rk, #]] &, args];

HoldPattern[RungeKuttaLinearStabilityP[method: RungeKutta[A_, b_, __], z_]] := Det[IdentityMatrix[Length[A]] + z * (ConstantArray[b, Length[A]] - A)];

HoldPattern[RungeKuttaLinearStabilityQ[RungeKutta[A_, __], z_]] := Det[IdentityMatrix[Length[A]] - z * A];

RungeKuttaLinearStabilityE[rk_RungeKutta, y_] := ComplexExpand[
	RungeKuttaLinearStabilityQ[rk, y * I] * RungeKuttaLinearStabilityQ[rk, -y * I]
	- RungeKuttaLinearStabilityP[rk, y * I] * RungeKuttaLinearStabilityP[rk, -y * I]
];

RungeKuttaAStableCondition[rk_RungeKutta] := Resolve[ForAll[y, RungeKuttaLinearStabilityE[rk, y] >= 0], Reals]

RungeKuttaStifflyAccurateCondition[rk_RungeKutta] := And @@ Thread[Last[RungeKuttaA[rk]] == RungeKuttaB[rk]];


End[];


EndPackage[];
