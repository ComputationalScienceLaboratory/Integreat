(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Rk`Simplify`"];


Integreat`Rk`Simplify::usage = "Package containing functions for simplifying and reducing Runge-Kutta methods";

RkDJReduce::usage = "Removes unused stages from a Runge-Kutta method";
RkDJReducibleQ::usage = "Returns True is a Runge-Kutta method is DJ-reducible and False otherwise";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Needs["Integreat`Rk`Methods`"];

RkDJIrreducibleStages[rk_] := With[{
		s = RkStages[rk],
		graph = Graph[rk]
	},
	Sort[Select[VertexOutComponent[graph, Range[s + 1, VertexCount[graph]]], # <= s &]]
];

RkSubset[rk_, {}] := RkSubset[rk, {1}];
RkSubset[rk_, p_] := Rk[
	RkA[rk][[p, p]],
	RkDenseOutput[rk][[p]],
	RkC[rk][[p]],
	If[RkPairQ[rk], RkBHat[rk][[p]], Unevaluated[Sequence[]]]
]


(* ::Section:: *)
(*Package Definitions*)


RkDJReduce[rk_Rk] := RkSubset[rk, RkDJIrreducibleStages[rk]];

RkDJReducibleQ[rk_Rk] := Length[RkDJIrreducibleStages[rk]] =!= RkStages[rk];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
