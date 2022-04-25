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

rkLinearStability[rk_, lim_DirectedInfinity, stages_, opts___] := Limit[rkLinearStability[rk, z, stages, opts], z -> lim];
rkLinearStability[rk_, z_, None, opts___] := 1 + z * RKB[rk, opts] . rkLinearStability[rk, z, All];
rkLinearStability[rk_, z_, stages_, ___] := Total[Inverse[IdentityMatrix[RKStages[rk]] - z * RKA[rk]], {2}][[stages]];


(* ::Section:: *)
(*Package Definitions*)


RKLinearStability[rk_RK, z_, opts:OptionsPattern[RKB]] := rkLinearStability[rk, z, OptionValue[Stage], opts];

RKOrderStarPlot[rk_RK, re:{_, _}:{-4, 4}, im:{_, _}:{-4, 4}, opts:OptionsPattern[{RKB, RegionPlot}]] := OrderStarPlot[
	Evaluate[Abs[RKLinearStability[rk, #, FilterRules[{opts}, Options[RKB]]]]] &,
	re,
	im,
	FilterRules[{opts}, Options[RegionPlot]]
];

RKLinearStabilityPlot[rk_RK, re:{_, _}:{-6, 2}, im:{_, _}:{-4, 4}, opts:OptionsPattern[{RKB, RegionPlot}]] := LinearStabilityPlot[
	Evaluate[Abs[RKLinearStability[rk, #, FilterRules[{opts}, Options[RKB]]]]] &,
	re,
	im,
	FilterRules[{opts}, Options[RegionPlot]]
];

RKLinearStabilityP[rk_RK, z_, opts:OptionsPattern[RKB]] := Det[
	IdentityMatrix[RKStages[rk]] + z * (ConstantArray[RKB[rk, opts], RKStages[rk]] - RKA[rk])
];

RKLinearStabilityQ[rk_RK, z_] := Det[IdentityMatrix[RKStages[rk]] - z * RKA[rk]];

RKEPolynomial[rk_RK, y_, opts:OptionsPattern[RKB]] := ComplexExpand[
	RKLinearStabilityQ[rk, y * I] * RKLinearStabilityQ[rk, -y * I]
	- RKLinearStabilityP[rk, y * I, opts] * RKLinearStabilityP[rk, -y * I, opts]
];

RKAStableCondition[rk_RK, opts:OptionsPattern[RKB]] := Resolve[ForAll[y, RKEPolynomial[rk, y, opts] >= 0], Reals]

RKStifflyAccurateQ[rk_RK] := VectorQ[Last[RKA[rk]] - RKB[rk], PossibleZeroQ];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
