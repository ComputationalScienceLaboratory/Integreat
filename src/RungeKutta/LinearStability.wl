(* ::Package:: *)

BeginPackage["CSL`OdeUtils`RungeKutta`LinearStability`"];


CSL`OdeUtils`RungeKutta`LinearStability::usage = "Package containing functions for analyzing the linear stability of Runge-Kutta methods";

RungeKuttaLinearStability::usage = "The linear stability function for a Runge-Kutta method";
RungeKuttaOrderStarPlot::usage = "Plots the order star";
RungeKuttaLinearStabilityPlot::usage = "Plots the region of linear stability";
RungeKuttaLinearStabilityP::usage = "The numerator of the linear stability function";
RungeKuttaLinearStabilityQ::usage = "The denominator of the linear stability function";
RungeKuttaLinearStabilityE::usage = "The E-polynomial to test for I-stability";
RungeKuttaAStableCondition::usage = "Returns True if the Runge-Kutta method is A-stable, and False otherwise";
RungeKuttaStifflyAccurateCondition::usage = "Determines if a Runge-Kutta method is stiffly-accurate";


Begin["`Private`"];
Needs["CSL`OdeUtils`RungeKutta`Methods`"];
Needs["CSL`OdeUtils`Internal`LinearStability`"];


RungeKuttaLinearStability[rk_RungeKutta, z_, p_] := With[{
		s = Length[rk]
	},
	(*LinearSolve[IdentityMatrix[s] - z * RungeKuttaA[rk], ConstantArray[1, s]][[p]]*)
	(Inverse[IdentityMatrix[s] - z * RungeKuttaA[rk]].ConstantArray[1, s])[[p]]
];
RungeKuttaLinearStability[rk_RungeKutta, z_] := 1 + z * RungeKuttaB[rk].RungeKuttaLinearStability[rk, z, All];

RungeKuttaOrderStarPlot[rk_RungeKutta, args___] := OrderStarPlot[Evaluate[Abs[RungeKuttaLinearStability[rk, #]]] &, args];

RungeKuttaLinearStabilityPlot[rk_RungeKutta, args___] := LinearStabilityPlot[Evaluate[Abs[RungeKuttaLinearStability[rk, #]]] &, args];

RungeKuttaLinearStabilityP[rk_RungeKutta, z_] := With[{
		s = Length[rk]
	},
	Det[IdentityMatrix[s] + z * (ConstantArray[RungeKuttaB[rk], s] - RungeKuttaA[rk])]
];

RungeKuttaLinearStabilityQ[rk_RungeKutta, z_] := Det[IdentityMatrix[Length[rk]] - z * RungeKuttaA[rk]];

RungeKuttaLinearStabilityE[rk_RungeKutta, y_, p_|PatternSequence[]] := ComplexExpand[
	RungeKuttaLinearStabilityQ[rk, y * I, p] * RungeKuttaLinearStabilityQ[rk, -y * I, p]
	- RungeKuttaLinearStabilityP[rk, y * I, p] * RungeKuttaLinearStabilityP[rk, -y * I, p]
];

RungeKuttaAStableCondition[rk_RungeKutta] := Resolve[ForAll[y, RungeKuttaLinearStabilityE[rk, y] >= 0], Reals]

RungeKuttaStifflyAccurateCondition[rk_RungeKutta] := And @@ Thread[Last[RungeKuttaA[rk]] == RungeKuttaB[rk]];


End[];


EndPackage[];
