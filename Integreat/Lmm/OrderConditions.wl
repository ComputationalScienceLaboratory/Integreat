(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`OrderConditions`"];
Integreat`Lmm`OrderConditions::usage = "Package containing functions for determining the order of linear multistep methods";

LmmOrderConditions::usage = "?";
LmmOrder::usage = "?";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`Internal`MathUtils`", "Integreat`Lmm`Methods`"}];


(* ::Section:: *)
(*Package Definitions*)


LmmOrderConditions[lmm_Lmm, p_Integer?NonNegative] := With[{
		i = Range[0, LmmSteps[lmm]]
	},
	LmmAlpha[lmm].SeriesVander[i, 0, p] - LmmBeta[lmm].SeriesVander[i, -1, p - 1]
];

LmmOrder[lmm_Lmm] := With[{
		a = LmmAlpha[lmm],
		b = LmmBeta[lmm],
		i = Range[0, LmmSteps[lmm]]
	},
	NestWhile[# + 1 &, -1, PossibleZeroQ[a.SeriesVander[i, # + 1] - b.SeriesVander[i, #]] &]
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
