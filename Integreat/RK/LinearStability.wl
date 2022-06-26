(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RK`LinearStability`"];
Integreat`RK`LinearStability::usage = "This package contains functions for analyzing the stability of Runge-Kutta methods applied to the linear test problem y' = \[Lambda] y.";

RKLinearStability::usage =
	"RKLinearStability[rk, z] evaluates the linear stability function of rk at z = h \[Lambda]. If z is a DirectedInfinity, then the value in the limit is returned.\n" <>
	"RKLinearStability[rk, z, Stage \[Rule] All] evaluates the internal stability function of rk at z.\n" <>
	"RKLinearStability[\[Ellipsis], opts] evaluates using RKB options opts.";
RKLinearStabilityPlot::usage =
	"RKLinearStabilityPlot[rk] plots the linear stability region of rk.\n" <>
	"RKLinearStabilityPlot[rk, z] plots in the square region -z-z*I to z+z*I.\n" <>
	"RKLinearStabilityPlot[rk, {zMin, zMax}] plots in a region with corners specified by the complex numbers zMin and zMax.\n" <>
	"RKLinearStabilityPlot[\[Ellipsis], opts] plots using ComplexRegionPlot and RKB options opts.";
RKOrderStarPlot::usage =
	"RKOrderStarPlot[rk] plots the order star of rk.\n" <>
	"RKOrderStarPlot[rk, z] plots in the square region -z-z*I to z+z*I.\n" <>
	"RKOrderStarPlot[rk, {zMin, zMax}] plots in a region with corners specified by the complex numbers zMin and zMax.\n" <>
	"RKOrderStarPlot[\[Ellipsis], opts] plots using ComplexRegionPlot and RKB options opts.";
RKLinearStabilityP::usage =
	"RKLinearStabilityP[rk, z] evaluates the numerator of the linear stability function of rk at z.\n" <>
	"RKLinearStabilityP[\[Ellipsis], opts] evaluates using RKB options opts.";
RKLinearStabilityQ::usage = "RKLinearStabilityQ[rk, z] evaluates the denominator of the linear stability function of rk at z.";
RKEPolynomial::usage =
	"RKEPolynomial[rk, y] computes |Q(y * I)|^2 - |P(y * I)|^2 where Q and P are the denominator and numerator, respectively, of the linear stability function of rk.\n" <>
	"RKEPolynomial[\[Ellipsis], opts] computes the E-polynomial using RKB options opts.";
RKIStableCondition::usage =
	"RKIStableCondition[rk] returns an algebraic condition equivalent to rk being stable on the imaginary axis.\n" <>
	"RKIStableCondition[\[Ellipsis], opts] returns the I-stability condition using RKB options opts.";
RKAStableCondition::usage =
	"RKAStableCondition[rk] returns an algebraic condition equivalent to rk being stable in the left half-plane.\n" <>
	"RKAStableCondition[\[Ellipsis], opts] returns the A-stability condition using RKB options opts.";
RKStifflyAccurateQ::usage =
	"RKStifflyAccurateQ[rk] returns True if, for the coefficients of rk, the last row of A equals b. It returns False, otherwise.\n" <>
	"RKStifflyAccurateQ[\[Ellipsis], opts] uses RKB options opts.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`RK`Methods`", "Integreat`Internal`MathUtils`"}];

rkLinearStability[rk_, lim_DirectedInfinity, stages_, opts___] := Limit[rkLinearStability[rk, z, stages, opts], z -> lim];
rkLinearStability[rk_, z_, None, opts___] := 1 + z * RKB[rk, opts] . rkLinearStability[rk, z, All];
rkLinearStability[rk_, z_, stages_, ___] := Total[Inverse[IdentityMatrix[RKStages[rk]] - z * RKA[rk]], {2}][[stages]];

stabilityPlot[rk_, ref_, bounds_, opts___] := With[{
		stab = Abs[RKLinearStability[rk, z, FilterRules[{opts}, Options[RKB]]]] <= ref,
		plotOpts = FilterRules[{opts}, Except[Options[RKB]]]
	},
	ComplexRegionPlot[stab, bounds, plotOpts, FrameLabel -> {"Re", "Im"}]
];


(* ::Section:: *)
(*Package Definitions*)


RKLinearStability[rk_RK, z_, opts:OptionsPattern[RKB]] := rkLinearStability[rk, z, OptionValue[Stage], opts];

RKLinearStabilityPlot[
	rk_RK,
	Optional[{zMin_?NumericQ, zMax_?NumericQ} | zMin_?NumericQ, {-6 - 4I, 2 + 4I}],
	opts:OptionsPattern[{RKB, ComplexRegionPlot}]
] := stabilityPlot[rk, 1, {z, zMin, zMax}, opts];

RKOrderStarPlot[
	rk_RK,
	Optional[{zMin_?NumericQ, zMax_?NumericQ} | zMin_?NumericQ, 4],
	opts:OptionsPattern[{RKB, ComplexRegionPlot}]
] := stabilityPlot[rk, Exp[Re[z]], {z, zMin, zMax}, opts];

RKLinearStabilityP[rk_RK, z_, opts:OptionsPattern[RKB]] := Det[
	IdentityMatrix[RKStages[rk]] + z * (ConstantArray[RKB[rk, opts], RKStages[rk]] - RKA[rk])
];

RKLinearStabilityQ[rk_RK, z_] := Det[IdentityMatrix[RKStages[rk]] - z * RKA[rk]];

RKEPolynomial[rk_RK, y_, opts:OptionsPattern[RKB]] := ComplexExpand[
	Total[ReIm[RKLinearStabilityQ[rk, y * I]]^2 - ReIm[RKLinearStabilityP[rk, y * I, opts]]^2]
];

RKIStableCondition[rk_RK, opts:OptionsPattern[RKB]] := Resolve[ForAll[y, RKEPolynomial[rk, y, opts] >= 0], Reals];

RKAStableCondition[rk_RK, opts:OptionsPattern[RKB]] := And[
	RKIStableCondition[rk, opts],
	FunctionAnalytic[{RKLinearStability[rk, z], Re[z] < 0}, z, Complexes]
];

RKStifflyAccurateQ[rk_RK] := VectorQ[Last[RKA[rk]] - RKB[rk], ZeroQ];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
