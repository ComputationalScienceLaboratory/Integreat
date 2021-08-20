(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RK`Simplify`"];


Integreat`RK`Simplify::usage = "Package containing functions for simplifying and reducing Runge-Kutta methods";

RKDJReduce::usage = "Removes unused stages from a Runge-Kutta method";
RKDJReducibleQ::usage = "Returns True is a Runge-Kutta method is DJ-reducible and False otherwise";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`RK`Methods`"];

RKDJIrreducibleStages[rk_] := With[{
		s = RKStages[rk],
		graph = Graph[rk]
	},
	Sort[Select[VertexOutComponent[graph, Range[s + 1, VertexCount[graph]]], # <= s &]]
];

RKSubset[rk_, {}] := RKSubset[rk, {1}];
RKSubset[rk_, p_] := RK[
	RKA[rk][[p, p]],
	RKDenseOutput[rk][[p]],
	RKC[rk][[p]],
	If[RKPairQ[rk], RKBHat[rk][[p]], Unevaluated[Sequence[]]]
]


(* ::Section:: *)
(*Package Definitions*)


RKDJReduce[rk_RK] := RKSubset[rk, RKDJIrreducibleStages[rk]];

RKDJReducibleQ[rk_RK] := Length[RKDJIrreducibleStages[rk]] =!= RKStages[rk];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
