(* ::Package:: *)

BeginPackage["Integreat`Rk`NonlinearStability`"];


Integreat`Rk`NonlinearStability::usage = "Package containing functions for analyzing the nonlinear stability of Runge-Kutta methods";

RkAlgebraicStabilityMatrix::usage = "The algebraic stability matrix of a Runge-Kutta method";
RkAlgebraicallyStableQ::usage = "Returns True if the Runge-Kutta method is algebraically stable and False otherwise";
RkSymplecticQ::usage = "Returns True if the Runge-Kutta method is symplectic and False otherwise";


Begin["`Private`"];
Needs["Integreat`Rk`Methods`"];

AlgMat[A_, b_, s_] := Table[b[[i]] * A[[i,j]] + b[[j]] * A[[j,i]] - b[[i]] * b[[j]], {i, s}, {j, s}]


RkAlgebraicStabilityMatrix[rk_Rk, p_Integer] := With[{
		A = RkA[rk]
	},
	AlgMat[A, A[[p]], RkStages[rk]]
];
RkAlgebraicStabilityMatrix[rk_Rk] := AlgMat[RkA[rk], RkB[rk], RkStages[rk]];

RkAlgebraicallyStableQ[rk_Rk, p_Integer | PatternSequence[]] := PositiveSemidefiniteMatrixQ[RkAlgebraicStabilityMatrix[rk, p]];

RkSymplecticQ[rk_Rk, p_Integer | PatternSequence[]] := MatrixQ[RkAlgebraicStabilityMatrix[rk, p], PossibleZeroQ];


End[];
EndPackage[];
