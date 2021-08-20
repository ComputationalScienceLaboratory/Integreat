(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RK`Symmetry`"];
Integreat`RK`Symmetry::usage = "Package containing functions for simplifying and reducing Runge-Kutta methods";

RKReflection::usage = "The Runge-Kutta method from taking a step backward in time then swapping the input and output states";
RKTranspose::usage = "The discrete adjoint of the Runge-Kutta method";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`RK`Methods`"];


(* ::Section:: *)
(*Package Definitions*)


RKReflection[rk:HoldPattern[RK[A_, b_, c_, d___]]] := RK[ConstantArray[RKB[rk], Length[b]] - A, b, 1 - c, d];

RKTranspose[rk:HoldPattern[RK[A_, bdo_, c_, d___]]] /; NoneTrue[RKB[rk], PossibleZeroQ] := With[{
		s = RKStages[rk],
		b = RKB[rk]
	},
	RK[Table[b[[j]] * A[[j, i]] / b[[i]], {i, s}, {j, s}], bdo, 1 - c, d]
];

RK /: Transpose[rk_RK] := RKTranspose[rk];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
