(* ::Package:: *)

BeginPackage["Integreat`LMM`OrderConditions`", {"Integreat`Internal`MathUtils`", "Integreat`LMM`Methods`"}];


Integreat`LMM`OrderConditions::usage = "Package containing functions for determining the order of linear multistep methods";

LmmOrderConditions::usage = "?";
LmmOrder::usage = "?";


Begin["`Private`"];

LmmOrderConditions[lmm_Lmm, p_Integer?NonNegative] := With[{
		i = Range[0, Length[lmm]]
	},
	ThreadEqual[LmmAlpha[lmm].SeriesVander[i, 0, p], LmmBeta[lmm].SeriesVander[i, -1, p - 1]]
];

LmmOrder[lmm_Lmm] := With[{
		a = LmmAlpha[lmm],
		b = LmmBeta[lmm],
		i = Range[0, Length[lmm]]
	},
	NestWhile[# + 1 &, -1, PossibleZeroQ[a.SeriesVander[i, # + 1] - b.SeriesVander[i, #]] &]
];


End[];
EndPackage[];
