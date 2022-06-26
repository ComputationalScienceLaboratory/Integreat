(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RK`Simplify`"];


Integreat`RK`Simplify::usage = "Package containing functions for simplifying and reducing Runge-Kutta methods";

RKDJReduce::usage = "RKDJReduce[rk] returns a new Runge-Kutta method with the unused stages of rk removed.";
RKDJReducibleQ::usage = "RKDJReducibleQ[rk] returns True if rk has unused stages and False, otherwise";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`RK`Methods`"];

RKDJIrreducibleStages[rk_] := With[{
		s = RKStages[rk]
	},
	Select[VertexOutComponent[Graph[rk], v_ /; v > s], LessEqualThan[s]]
];

RKSubset[rk_, {}] := RKSubset[rk, {1}];
RKSubset[HoldPattern[RK[a_, b___]], p_] := RK[
	a[[p, p]],
	Sequence @@ {b}[[All, p]]
]


(* ::Section:: *)
(*Package Definitions*)


RKDJReduce[rk_RK] := RKSubset[rk, Sort[RKDJIrreducibleStages[rk]]];

RKDJReducibleQ[rk_RK] := Length[RKDJIrreducibleStages[rk]] =!= RKStages[rk];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
