(* ::Package:: *)

BeginPackage["Integreat`RungeKutta`NonlinearStability`"];


Integreat`RungeKutta`NonlinearStability::usage = "Package containing functions for analyzing the nonlinear stability of Runge-Kutta methods";

RungeKuttaAlgebraicStabilityMatrix::usage = "The algebraic stability matrix of a Runge-Kutta method";
RungeKuttaAlgebraicallyStableQ::usage = "Returns True if the Runge-Kutta method is algebraically stable and False otherwise";
RungeKuttaSymplecticCondition::usage = "The condition for the Runge-Kutta method to by symplectic";


Begin["`Private`"];
Needs["Integreat`RungeKutta`Methods`"];

AlgMat[A_, b_, s_] := Table[b[[i]] * A[[i,j]] + b[[j]] * A[[j,i]] - b[[i]] * b[[j]], {i, s}, {j, s}]


RungeKuttaAlgebraicStabilityMatrix[rk_RungeKutta, p_Integer] := With[{
		A = RungeKuttaA[rk]
	},
	AlgMat[A, A[[p]], Length[rk]]
];
RungeKuttaAlgebraicStabilityMatrix[rk_RungeKutta] := AlgMat[RungeKuttaA[rk], RungeKuttaB[rk], Length[rk]];

RungeKuttaAlgebraicallyStableQ[rk_RungeKutta, p_Integer | PatternSequence[]] := PositiveSemidefiniteMatrixQ[RungeKuttaAlgebraicStabilityMatrix[rk, p]];

RungeKuttaSymplecticCondition[rk_RungeKutta, p_Integer | PatternSequence[]] := And @@ Thread[Flatten[RungeKuttaAlgebraicStabilityMatrix[rk, p]] == 0];


End[];


EndPackage[];
