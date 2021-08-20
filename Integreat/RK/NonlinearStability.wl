(* ::Package:: *)

BeginPackage["Integreat`RK`NonlinearStability`"];


Integreat`RK`NonlinearStability::usage = "Package containing functions for analyzing the nonlinear stability of Runge-Kutta methods";

RKAlgebraicStabilityMatrix::usage = "The algebraic stability matrix of a Runge-Kutta method";
RKAlgebraicallyStableQ::usage = "Returns True if the Runge-Kutta method is algebraically stable and False otherwise";
RKSymplecticQ::usage = "Returns True if the Runge-Kutta method is symplectic and False otherwise";


Begin["`Private`"];
Needs["Integreat`RK`Methods`"];

AlgMat[A_, b_, s_] := Table[b[[i]] * A[[i,j]] + b[[j]] * A[[j,i]] - b[[i]] * b[[j]], {i, s}, {j, s}]


RKAlgebraicStabilityMatrix[rk_RK, p_Integer] := With[{
		A = RKA[rk]
	},
	AlgMat[A, A[[p]], RKStages[rk]]
];
RKAlgebraicStabilityMatrix[rk_RK, opts:OptionsPattern[RKB]] := AlgMat[RKA[rk], RKB[rk, opts], RKStages[rk]];

RKAlgebraicallyStableQ[rk_RK, args:(_Integer | OptionsPattern[RKB])] := PositiveSemidefiniteMatrixQ[RKAlgebraicStabilityMatrix[rk, args]];

RKSymplecticQ[rk_RK, args:(_Integer | OptionsPattern[RKB])] := MatrixQ[RKAlgebraicStabilityMatrix[rk, args], PossibleZeroQ];


End[];
EndPackage[];
