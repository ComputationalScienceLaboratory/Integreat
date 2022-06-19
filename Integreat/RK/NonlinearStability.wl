(* ::Package:: *)

BeginPackage["Integreat`RK`NonlinearStability`"];


Integreat`RK`NonlinearStability::usage = "Package containing functions for analyzing the nonlinear stability of Runge-Kutta methods";

RKAlgebraicStabilityMatrix::usage = "The algebraic stability matrix of a Runge-Kutta method";
RKAlgebraicallyStableQ::usage = "Returns True if the Runge-Kutta method is algebraically stable and False otherwise";
RKSymplecticQ::usage = "Returns True if the Runge-Kutta method is symplectic and False otherwise";
RKAbsoluteMonotonicityRadius::usage = "Radius of absolute monotonicty";


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
