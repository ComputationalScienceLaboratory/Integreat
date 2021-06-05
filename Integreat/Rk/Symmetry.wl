(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Rk`Symmetry`"];
Integreat`Rk`Symmetry::usage = "Package containing functions for simplifying and reducing Runge-Kutta methods";

RkReflection::usage = "The Runge-Kutta method from taking a step backward in time then swapping the input and output states";
RkTranspose::usage = "The discrete adjoint of the Runge-Kutta method";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`Rk`Methods`"];


(* ::Section:: *)
(*Package Definitions*)


RkReflection[rk:HoldPattern[Rk[A_, b_, c_, d___]]] := Rk[ConstantArray[RkB[rk], Length[b]] - A, b, 1 - c, d];

(*RkSymmetric[rk*)

RkTranspose[rk:HoldPattern[Rk[A_, bdo_, c_, d___]]] /; NoneTrue[RkB[rk], PossibleZeroQ] := With[{
		s = Length[rk],
		b = RkB[rk]
	},
	Rk[Table[b[[j]] * A[[j, i]] / b[[i]], {i, s}, {j, s}], bdo, 1 - c, d]
];

Rk /: Transpose[rk_Rk] := RkTranspose[rk];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
