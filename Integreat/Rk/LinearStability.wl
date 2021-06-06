(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Rk`LinearStability`"];
Integreat`Rk`LinearStability::usage = "Package containing functions for analyzing the linear stability of Runge-Kutta methods";

RkLinearStability::usage = "The linear stability function for a Runge-Kutta method applied to y'=\[Lambda]y";
RkOrderStarPlot::usage = "Plots the order star";
RkLinearStabilityPlot::usage = "Plots the region of linear stability";
RkLinearStabilityP::usage = "The numerator of the linear stability function";
RkLinearStabilityQ::usage = "The denominator of the linear stability function";
RkEPolynomial::usage = "The E-polynomial to test for I-stability";
RkAStableCondition::usage = "Returns True if the Runge-Kutta method is A-stable, and False otherwise";
RkStifflyAccurateQ::usage = "Determines if a Runge-Kutta method is stiffly-accurate";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`Rk`Methods`", "Integreat`Internal`LinearStability`"}];

StabilityNumerator[A_, b_, s_, z_] := Det[IdentityMatrix[s] + z * (ConstantArray[b, s] - A)];


(* ::Section:: *)
(*Package Definitions*)


RkLinearStability[rk_Rk, lim_DirectedInfinity, p_ | PatternSequence[]] := Limit[RkLinearStability[rk, z, p], z -> lim];
RkLinearStability[rk_Rk, z_, p_] := Total[Inverse[IdentityMatrix[Length[rk]] - z * RkA[rk]], {2}][[p]];
RkLinearStability[rk_Rk, z_] := 1 + z * RkB[rk].RkLinearStability[rk, z, All];

RkOrderStarPlot[rk_Rk, args___] := OrderStarPlot[Evaluate[Abs[RkLinearStability[rk, #]]] &, args];

RkLinearStabilityPlot[rk_Rk, args___] := LinearStabilityPlot[Evaluate[Abs[RkLinearStability[rk, #]]] &, args];

RkLinearStabilityP[rk_Rk, z_, p_Integer] := With[{
		A = RkA[rk]
	},
	StabilityNumerator[A, A[[p]], Length[rk], z]
];

RkLinearStabilityP[rk_Rk, z_] := StabilityNumerator[RkA[rk], RkB[rk], Length[rk], z];

RkLinearStabilityQ[rk_Rk, z_] := Det[IdentityMatrix[Length[rk]] - z * RkA[rk]];

RkEPolynomial[rk_Rk, y_, p_Integer | PatternSequence[]] := ComplexExpand[
	RkLinearStabilityQ[rk, y * I] * RkLinearStabilityQ[rk, -y * I]
	- RkLinearStabilityP[rk, y * I, p] * RkLinearStabilityP[rk, -y * I, p]
];

RkAStableCondition[rk_Rk, p_Integer | PatternSequence[]] := Resolve[ForAll[y, RkEPolynomial[rk, y, p] >= 0], Reals]

RkStifflyAccurateQ[rk_Rk] := VectorQ[Last[RkA[rk]] - RkB[rk], PossibleZeroQ];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
