(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


rkDJIrreducibleStages[rk_] := With[{
		s = RKStages[rk]
	},
	Select[VertexOutComponent[Graph[rk], v_ /; v > s], LessEqualThan[s]]
];


rkSubset[rk_, {}] := RKSubset[rk, {1}];
rkSubset[HoldPattern[RK[a_, b___]], p_] := RK[
	a[[p, p]],
	Sequence @@ {b}[[All, p]]
]


(* ::Section:: *)
(*Package Definitions*)


RKDJReduce[rk_RK] := rkSubset[rk, Sort[rkDJIrreducibleStages[rk]]];


RKDJReducibleQ[rk_RK] := Length[rkDJIrreducibleStages[rk]] =!= RKStages[rk];
