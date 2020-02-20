(* ::Package:: *)

BeginPackage["Integreat`RungeKutta`NonlinearStability`"];


CSL`OdeUtils`RungeKutta`NonlinearStability::usage = "Package containing functions for analyzing the nonlinear stability of Runge-Kutta methods";

RungeKuttaAlgebraicStabilityMatrix::usage = "The algebraic stability matrix of a Runge-Kutta method";
RungeKuttaAlgebraicallyStableQ::usage = "Returns True if the Runge-Kutta method is algebraically stable and False otherwise";
RungeKuttaSymplecticCondition::usage = "The condition for the Runge-Kutta method to by symplectic";


Begin["`Private`"];
Needs["Integreat`RungeKutta`Methods`"];


RungeKuttaAlgebraicStabilityMatrix[rk_RungeKutta] := With[{
	A = RungeKuttaA[rk],
	b = RungeKuttaB[rk],
	s = Length[rk]
},
	Table[b[[i]] * A[[i,j]] + b[[j]] * A[[j,i]] - b[[i]] * b[[j]], {i, s}, {j, s}]
];

RungeKuttaAlgebraicallyStableQ[rk_RungeKutta] := PositiveSemidefiniteMatrixQ[RungeKuttaAlgebraicStabilityMatrix[rk]];

RungeKuttaSymplecticCondition[rk_RungeKutta] := And @@ Thread[Flatten[RungeKuttaAlgebraicStabilityMatrix[rk]] == 0];


End[];


EndPackage[];
