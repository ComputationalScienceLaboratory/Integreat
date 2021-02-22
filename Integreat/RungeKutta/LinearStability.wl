(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RungeKutta`LinearStability`"];
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
Scan[Needs, {"Integreat`RungeKutta`Methods`", "Integreat`Internal`LinearStability`"}];

StabilityNumerator[A_, b_, s_, z_] := Det[IdentityMatrix[s] + z * (ConstantArray[b, s] - A)];


(* ::Section:: *)
(*Package Definitions*)


RungeKuttaLinearStability[rk_RungeKutta, lim_DirectedInfinity, p_ | PatternSequence[]] := Limit[RungeKuttaLinearStability[rk, z, p], z -> lim];
RungeKuttaLinearStability[rk_RungeKutta, z_, p_] := Total[Inverse[IdentityMatrix[Length[rk]] - z * RungeKuttaA[rk]], {2}][[p]];
RungeKuttaLinearStability[rk_RungeKutta, z_] := 1 + z * RungeKuttaB[rk].RungeKuttaLinearStability[rk, z, All];

RungeKuttaOrderStarPlot[rk_RungeKutta, args___] := OrderStarPlot[Evaluate[Abs[RungeKuttaLinearStability[rk, #]]] &, args];

RungeKuttaLinearStabilityPlot[rk_RungeKutta, args___] := LinearStabilityPlot[Evaluate[Abs[RungeKuttaLinearStability[rk, #]]] &, args];

RungeKuttaLinearStabilityP[rk_RungeKutta, z_, p_Integer] := With[{
		A = RungeKuttaA[rk]
	},
	StabilityNumerator[A, A[[p]], Length[rk], z]
];

RungeKuttaLinearStabilityP[rk_RungeKutta, z_] := StabilityNumerator[RungeKuttaA[rk], RungeKuttaB[rk], Length[rk], z];

RungeKuttaLinearStabilityQ[rk_RungeKutta, z_] := Det[IdentityMatrix[Length[rk]] - z * RungeKuttaA[rk]];

RungeKuttaEPolynomial[rk_RungeKutta, y_, p_Integer | PatternSequence[]] := ComplexExpand[
	RungeKuttaLinearStabilityQ[rk, y * I] * RungeKuttaLinearStabilityQ[rk, -y * I]
	- RungeKuttaLinearStabilityP[rk, y * I, p] * RungeKuttaLinearStabilityP[rk, -y * I, p]
];

RungeKuttaAStableCondition[rk_RungeKutta, p_Integer | PatternSequence[]] := Resolve[ForAll[y, RungeKuttaEPolynomial[rk, y, p] >= 0], Reals]

RungeKuttaStifflyAccurateCondition[rk_RungeKutta] := And @@ Thread[Last[RungeKuttaA[rk]] == RungeKuttaB[rk]];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
