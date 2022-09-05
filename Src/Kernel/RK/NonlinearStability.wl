(* ::Package:: *)

BeginPackage["Integreat`RK`NonlinearStability`"];


RKAlgebraicStabilityMatrix::usage = "RKAlgebraicStabilityMatrix[rk] computes the algebraic stability matrix of rk.";
RKAlgebraicallyStableQ::usage = "RKAlgebraicallyStableQ[rk] returns True if rk is algebraically stable and False, otherwise.";
RKSymplecticQ::usage = "RKSymplecticQ[rk] returns True if rk is symplectic and False, otherwise.";
RKAbsoluteMonotonicityRadius::usage = "RKAbsoluteMonotonicityRadius[rk] computes the radius of absolute monotonicty (SSP coefficient) of rk.";


Begin["`Private`"];
Scan[Needs, {"Integreat`RK`Core`", "Integreat`Internal`MathUtils`"}];

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
