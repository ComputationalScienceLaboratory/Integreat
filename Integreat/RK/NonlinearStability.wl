(* ::Package:: *)

BeginPackage["Integreat`RK`NonlinearStability`"];


RKAlgebraicStabilityMatrix::usage =
	"RKAlgebraicStabilityMatrix[rk] computes the algebraic stability matrix of rk.\n" <>
	"RKAlgebraicStabilityMatrix[\[Ellipsis], opts] computes the algebraic stability matrix using RKB options opts.";
RKAlgebraicallyStableQ::usage =
	"RKAlgebraicallyStableQ[rk] returns True if rk is algebraically stable and False, otherwise.\n" <>
	"RKAlgebraicallyStableQ[\[Ellipsis], opts] checks algebraic stability using RKB options opts.";
RKSymplecticQ::usage =
	"RKSymplecticQ[rk] returns True if rk is symplectic and False, otherwise.\n" <>
	"RKSymplecticQ[\[Ellipsis], opts] checks symplecticity using RKB options opts.";
RKAbsoluteMonotonicityRadius::usage = "RKAbsoluteMonotonicityRadius[rk] computes the radius of absolute monotonicty (SSP coefficient) of rk.\n" <>
	"RKAbsoluteMonotonicityRadius[\[Ellipsis], opts] computres the radius of absolute monotonicty using RKB options opts.";


Begin["`Private`"];
Scan[Needs, {"Integreat`RK`Methods`", "Integreat`Internal`MathUtils`"}];

AlgMat[A_, b_, s_] := Table[b[[i]] * A[[i,j]] + b[[j]] * A[[j,i]] - b[[i]] * b[[j]], {i, s}, {j, s}];

MonotonicityMat[A_, b_, s_, r_] := Inverse[IdentityMatrix[s + 1] - ArrayFlatten[{{r * A, 0}, {{r * b}, 0}}]] . ArrayFlatten[{{A, 1}, {{b}, 1}}];


RKAlgebraicStabilityMatrix[rk_RK, opts:OptionsPattern[RKB]] := AlgMat[RKA[rk], RKB[rk, opts], RKStages[rk]];

RKAlgebraicallyStableQ[rk_RK, opts:OptionsPattern[RKB]] := PositiveSemidefiniteMatrixQ[RKAlgebraicStabilityMatrix[rk, opts]];

RKSymplecticQ[rk_RK, opts:OptionsPattern[RKB]] := MatrixQ[RKAlgebraicStabilityMatrix[rk, opts], ZeroQ];

RKAbsoluteMonotonicityRadius[rk_RK, opts:OptionsPattern[RKB]] := PiecewiseExpand[Max[
	Quiet[
		MaxValue[{r, MonotonicityMat[RKA[rk], RKB[rk, opts], RKStages[rk], -r] >= 0}, r],
		MaxValue::infeas
	],
	0
]];


End[];
EndPackage[];
