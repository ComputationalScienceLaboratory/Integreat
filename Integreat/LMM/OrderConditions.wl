(* ::Package:: *)

BeginPackage["Integreat`LMM`OrderConditions`", {"Integreat`Internal`MathUtils`", "Integreat`LMM`Methods`"}];


Integreat`LMM`OrderConditions::usage = "Package containing functions for determining the order of linear multistep methods";

LmmOrderConditions::usage = "?";
LmmOrder::usage = "?";


Begin["`Private`"];

LmmOrderConditions[lmm_Lmm, p_Integer?NonPositive] := Thread[
	LmmAlpha[lmm].Table[Pow[i, j], {i, 0, Length[lmm]}, {j, 0, p}] == LmmBeta[lmm].Table[j * Pow[i, j - 1], {i, 0, Length[lmm]}, {j, 0, p}]
];

LmmOrder[lmm_Lmm] := With[{
		a = LmmAlpha[lmm],
		b = LmmBeta[lmm],
		i = Range[0, Length[lmm]]
	},
	NestWhile[# + 1 &, 0, PossibleZeroQ[a.Pow[i, #] - # * b.Pow[i, # - 1]] &]
];


End[];
EndPackage[];
