(* ::Package:: *)

BeginPackage["CSL`OdeUtils`RungeKutta`Symmetry`"];


CSL`OdeUtils`RungeKutta`Symmetry::usage = "Package containing functions for simplifying and reducing Runge-Kutta methods";

RungeKuttaAdjoint::usage = "The adjoint of a Runge-Kutta method";


Begin["`Private`"];
Needs["CSL`OdeUtils`RungeKutta`Methods`"];


RungeKuttaAdjoint[rk_RungeKutta] := With[{
		s = Length[rk],
		A = RungeKuttaA[rk],
		b = RungeKuttaB[rk]
	},
	RungeKutta[Table[b[[j]] - A[[i, j]], {i, s, 1, -1}, {j, s, 1, -1}], Reverse[b], 1 - Reverse[RungeKuttaC[rk]]]
];

RungeKutta /: Transpose[rk_RungeKutta] := RungeKuttaAdjoint[rk];

RungeKutta /: ConjugateTranspose[rk_RungeKutta] := RungeKuttaAdjoint[rk];


End[];


EndPackage[];
