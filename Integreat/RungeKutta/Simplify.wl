(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`RungeKutta`Simplify`", {"Integreat`RungeKutta`Methods`"}];


Integreat`RungeKutta`Simplify::usage = "Package containing functions for simplifying and reducing Runge-Kutta methods";

RungeKuttaDJReduce::usage = "Removes unused stages from a Runge-Kutta method";
RungeKuttaDJReducibleQ::usage = "Returns True is a Runge-Kutta method is DJ-reducible and False otherwise";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];

RungeKuttaDJIrreducibleStages[rk_] := With[{
		s = Length[rk],
		graph = Graph[rk]
	},
	Sort[Select[VertexOutComponent[graph, Range[s + 1, VertexCount[graph]]], # <= s &]]
];

RungeKuttaSubset[rk_, {}] := RungeKuttaSubset[rk, {1}];
RungeKuttaSubset[rk_, p_] := RungeKutta[
	RungeKuttaA[rk][[p, p]],
	RungeKuttaDenseOutput[rk][[p]],
	RungeKuttaC[rk][[p]],
	If[RungeKuttaPairQ[rk], RungeKuttaBHat[rk][[p]], Unevaluated[Sequence[]]]
]


(* ::Section:: *)
(*Package Definitions*)


RungeKuttaDJReduce[rk_RungeKutta] := RungeKuttaSubset[rk, RungeKuttaDJIrreducibleStages[rk]];

RungeKuttaDJReducibleQ[rk_RungeKutta] := Length[RungeKuttaDJIrreducibleStages[rk]] =!= Length[rk];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
