(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


rkLinearStability[rk_, lim_DirectedInfinity, stages_, opts___] := Limit[rkLinearStability[rk, z, stages, opts], z -> lim];
rkLinearStability[rk_, z_, None, opts___] := 1 + z * RKB[rk, opts] . rkLinearStability[rk, z, All];
rkLinearStability[rk_, z_, stages_, ___] := Total[Inverse[IdentityMatrix[RKStages[rk]] - z * RKA[rk]], {2}][[stages]];


rkStabilityPlot[rk_, comp_, bounds_, opts___] := With[{
		stab = comp[Abs[RKLinearStability[rk, z, FilterRules[{opts}, Options[RKB]]]]],
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
] := rkStabilityPlot[rk, LessEqualThan[1], {z, zMin, zMax}, opts];


RKOrderStarPlot[
	rk_RK,
	Optional[{zMin_?NumericQ, zMax_?NumericQ} | zMin_?NumericQ, 4],
	opts:OptionsPattern[{RKB, ComplexRegionPlot}]
] := rkStabilityPlot[rk, GreaterThan[Exp[Re[one[rk, opts] * z]]], {z, zMin, zMax}, opts];


RKLinearStabilityP[rk_RK, z_, opts:OptionsPattern[RKB]] := Det[
	IdentityMatrix[RKStages[rk]] + z * (ConstantArray[RKB[rk, opts], RKStages[rk]] - RKA[rk])
];


RKLinearStabilityQ[rk_RK, z_] := Det[IdentityMatrix[RKStages[rk]] - z * RKA[rk]];


RKEPolynomial[rk_RK, y_, opts:OptionsPattern[RKB]] := ComplexExpand[
	Total[ReIm[RKLinearStabilityQ[rk, y * I]]^2 - ReIm[RKLinearStabilityP[rk, y * I, opts]]^2]
];


RKIStable[rk_RK, opts:OptionsPattern[RKB]] := Resolve[ForAll[y, RKEPolynomial[rk, y, opts] >= 0], Reals];


RKAStable[rk_RK, opts:OptionsPattern[RKB]] := And[
	RKIStable[rk, opts],
	FunctionAnalytic[{RKLinearStability[rk, z], Re[z] < 0}, z, Complexes]
];
