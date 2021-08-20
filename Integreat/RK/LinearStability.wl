(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RK`LinearStability`"];
Integreat`RK`LinearStability::usage = "Package containing functions for analyzing the linear stability of Runge-Kutta methods";

RKLinearStability::usage = "The linear stability function for a Runge-Kutta method applied to y'=\[Lambda]y";
RKOrderStarPlot::usage = "Plots the order star";
RKLinearStabilityPlot::usage = "Plots the region of linear stability";
RKLinearStabilityP::usage = "The numerator of the linear stability function";
RKLinearStabilityQ::usage = "The denominator of the linear stability function";
RKEPolynomial::usage = "The E-polynomial to test for I-stability";
RKAStableCondition::usage = "Returns True if the Runge-Kutta method is A-stable, and False otherwise";
RKStifflyAccurateQ::usage = "Determines if a Runge-Kutta method is stiffly-accurate";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`RK`Methods`", "Integreat`Internal`LinearStability`"}];

stabilityNumerator[A_, b_, s_, z_] := Det[IdentityMatrix[s] + z * (ConstantArray[b, s] - A)];


(* ::Section:: *)
(*Package Definitions*)


RKLinearStability[rk_RK, lim_DirectedInfinity, args:(All | _Integer | OptionsPattern[RKB])] := Limit[RKLinearStability[rk, z, args], z -> lim];
RKLinearStability[rk_RK, z_, p:(All | _Integer)] := Total[Inverse[IdentityMatrix[RKStages[rk]] - z * RKA[rk]], {2}][[p]];
RKLinearStability[rk_RK, z_, opts:OptionsPattern[RKB]] := 1 + z * RKB[rk, opts] . RKLinearStability[rk, z, All];

RKOrderStarPlot[rk_RK, re:{_, _}:{-4, 4}, im:{_, _}:{-4, 4}, opts:OptionsPattern[{RKB, RegionPlot}]] := With[{
		rkOpts = FilterRules[{opts}, Options[RKB]]
	},
	OrderStarPlot[Evaluate[Abs[RKLinearStability[rk, #, rkOpts]]] &, re, im, FilterRules[{opts}, Options[RegionPlot]]]
];

RKLinearStabilityPlot[rk_RK, re:{_, _}:{-6, 2}, im:{_, _}:{-4, 4}, opts:OptionsPattern[{RKB, RegionPlot}]] := With[{
		rkOpts = FilterRules[{opts}, Options[RKB]]
	},
	LinearStabilityPlot[Evaluate[Abs[RKLinearStability[rk, #, rkOpts]]] &, re, im, FilterRules[{opts}, Options[RegionPlot]]]
];

RKLinearStabilityP[rk_RK, z_, p_Integer] := With[{
		A = RKA[rk]
	},
	stabilityNumerator[A, A[[p]], RKStages[rk], z]
];

RKLinearStabilityP[rk_RK, z_, opts:OptionsPattern[RKB]] := stabilityNumerator[RKA[rk], RKB[rk, opts], RKStages[rk], z];

RKLinearStabilityQ[rk_RK, z_] := Det[IdentityMatrix[RKStages[rk]] - z * RKA[rk]];

RKEPolynomial[rk_RK, y_, args:(_Integer | OptionsPattern[RKB])] := ComplexExpand[
	RKLinearStabilityQ[rk, y * I] * RKLinearStabilityQ[rk, -y * I]
	- RKLinearStabilityP[rk, y * I, args] * RKLinearStabilityP[rk, -y * I, args]
];

RKAStableCondition[rk_RK, args:(_Integer | OptionsPattern[RKB])] := Resolve[ForAll[y, RKEPolynomial[rk, y, args] >= 0], Reals]

RKStifflyAccurateQ[rk_RK] := VectorQ[Last[RKA[rk]] - RKB[rk], PossibleZeroQ];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
