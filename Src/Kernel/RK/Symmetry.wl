(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RK`Symmetry`"];
Integreat`RK`Symmetry::usage = "Package containing functions for simplifying and reducing Runge-Kutta methods";

RKReflection::usage = "RKReflection[rk] returns a new Runge-Kutta method which is the reflection of rk. ";
RKTranspose::usage = "RKTranspose[rk] returns a new Runge-Kutta method which is the transpose of rk. ";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`RK`Core`", "Integreat`Internal`MathUtils`"}];


(* ::Section:: *)
(*Package Definitions*)


RKReflection[rk:HoldPattern[RK[A_, b_, c_, d___]]] := RK[ConstantArray[RKB[rk], Length[b]] - A, b, 1 - c, d];

RKTranspose[rk:HoldPattern[RK[A_, bdo_, c_, d___]]] /; NoneTrue[RKB[rk], ZeroQ] := With[{
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
