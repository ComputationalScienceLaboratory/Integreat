(* ::Package:: *)

BeginPackage["Integreat`RungeKutta`Symmetry`", {"Integreat`RungeKutta`Methods`"}];


Integreat`RungeKutta`Symmetry::usage = "Package containing functions for simplifying and reducing Runge-Kutta methods";

RungeKuttaReflection::usage = "The Runge-Kutta method from taking a step backward in time then swapping the input and output states";
RungeKuttaTranspose::usage = "The discrete adjoint of the Runge-Kutta method";


Begin["`Private`"];


RungeKuttaReflection[rk:HoldPattern[RungeKutta[A_, b_, c_, d___]]] := RungeKutta[ConstantArray[RungeKuttaB[rk], Length[rk]] - A, b, 1 - c, d];

(*RungeKuttaSymmetric[rk*)

RungeKuttaTranspose[rk:HoldPattern[RungeKutta[A_, bdo_, c_, d___]]] /; NoneTrue[RungeKuttaB[rk], PossibleZeroQ] := With[{
		s = Length[rk],
		b = RungeKuttaB[rk]
	},
	RungeKutta[Table[b[[j]] * A[[j, i]] / b[[i]], {i, s}, {j, s}], bdo, 1 - c, d]
];

RungeKutta /: Transpose[rk_RungeKutta] := RungeKuttaTranspose[rk];


End[];
EndPackage[];
