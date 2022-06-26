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
