(* ::Package:: *)

BeginPackage["CSL`OdeUtils`RungeKutta`NonlinearStability`"];


CSL`OdeUtils`RungeKutta`NonlinearStability::usage = "Package containing functions for analyzing the nonlinear stability of Runge-Kutta methods";

RungeKuttaAlgebraicStabilityMatrix::usage = "The algebraic stability matrix of a Runge-Kutta method";
RungeKuttaAlgebraicallyStableQ::usage = "Returns True if the Runge-Kutta method is algebraically stable and False otherwise";


Begin["`Private`"];
Needs["CSL`OdeUtils`RungeKutta`Methods`"];


RungeKuttaAlgebraicStabilityMatrix[rk_RungeKutta] := With[{
	A = RungeKuttaA[rk],
	b = RungeKuttaB[rk],
	s = RungeKuttaStages[rk]
},
	Table[b[[i]] * A[[i,j]] + b[[j]] * A[[j,i]] - b[[i]] * b[[j]], {i, s}, {j, s}]
];

RungeKuttaAlgebraicallyStableQ[rk_RungeKutta] := PositiveSemidefiniteMatrixQ[RungeKuttaAlgebraicStabilityMatrix[rk]];


End[];


EndPackage[];
