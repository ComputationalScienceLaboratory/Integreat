(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RungeKutta`LinearStability`", {"Integreat`RungeKutta`Methods`", "Integreat`Internal`LinearStability`"}];
Integreat`RungeKutta`LinearStability::usage = "Package containing functions for analyzing the linear stability of Runge-Kutta methods";

RungeKuttaLinearStability::usage = "The linear stability function for a Runge-Kutta method applied to y'=\[Lambda]y";
RungeKuttaOrderStarPlot::usage = "Plots the order star";
RungeKuttaLinearStabilityPlot::usage = "Plots the region of linear stability";
RungeKuttaLinearStabilityP::usage = "The numerator of the linear stability function";
RungeKuttaLinearStabilityQ::usage = "The denominator of the linear stability function";
RungeKuttaEPolynomial::usage = "The E-polynomial to test for I-stability";
RungeKuttaAStableCondition::usage = "Returns True if the Runge-Kutta method is A-stable, and False otherwise";
RungeKuttaStifflyAccurateCondition::usage = "Determines if a Runge-Kutta method is stiffly-accurate";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];


(* ::Section:: *)
(*Package Definitions*)


(*Inverse seems to be faster than LinearSolve*)
RungeKuttaLinearStability[rk_RungeKutta, Infinity, p:Repeated[_, {0, 1}]] := Limit[RungeKuttaLinearStability[rk, z, p], z -> Infinity];
RungeKuttaLinearStability[rk_RungeKutta, z_, p_] := Total[Inverse[IdentityMatrix[Length[rk]] - z * RungeKuttaA[rk]], {2}][[p]];
RungeKuttaLinearStability[rk_RungeKutta, z_] := 1 + z * RungeKuttaB[rk].RungeKuttaLinearStability[rk, z, All];

RungeKuttaOrderStarPlot[rk_RungeKutta, args___] := OrderStarPlot[Evaluate[Abs[RungeKuttaLinearStability[rk, #]]] &, args];

RungeKuttaLinearStabilityPlot[rk_RungeKutta, args___] := LinearStabilityPlot[Evaluate[Abs[RungeKuttaLinearStability[rk, #]]] &, args];

RungeKuttaLinearStabilityP[rk_RungeKutta, z_, p_] := With[{
		A = RungeKuttaA[rk],
		s = Length[rk]
	},
	(*Computes P for all stages.  More efficient implementation?*)
	Map[Det[IdentityMatrix[s] + z * (ConstantArray[#, s] - A)] &, A][[p]]
];

RungeKuttaLinearStabilityP[rk_RungeKutta, z_] := With[{
		s = Length[rk]
	},
	Det[IdentityMatrix[s] + z * (ConstantArray[RungeKuttaB[rk], s] - RungeKuttaA[rk])]
];

RungeKuttaLinearStabilityQ[rk_RungeKutta, z_] := Det[IdentityMatrix[Length[rk]] - z * RungeKuttaA[rk]];

RungeKuttaEPolynomial[rk_RungeKutta, y_] := ComplexExpand[
	RungeKuttaLinearStabilityQ[rk, y * I] * RungeKuttaLinearStabilityQ[rk, -y * I]
	- RungeKuttaLinearStabilityP[rk, y * I] * RungeKuttaLinearStabilityP[rk, -y * I]
];

RungeKuttaAStableCondition[rk_RungeKutta, p:Repeated[_, {0, 1}]] := Resolve[ForAll[y, RungeKuttaEPolynomial[rk, y, p] >= 0], Reals]

RungeKuttaStifflyAccurateCondition[rk_RungeKutta] := And @@ Thread[Last[RungeKuttaA[rk]] == RungeKuttaB[rk]];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
