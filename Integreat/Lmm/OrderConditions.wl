(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`Lmm`OrderConditions`"];
Integreat`Lmm`OrderConditions::usage = "Package containing functions for determining the order of linear multistep methods";

LmmOrderConditions::usage = "?";
LmmOrder::usage = "?";
LmmErrorConstant::usage = "?";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`Internal`MathUtils`", "Integreat`Lmm`Methods`"}];


(* ::Section:: *)
(*Package Definitions*)


LmmOrderConditions[lmm_Lmm, p_Integer?NonNegative] := With[{
		i = Range[0, LmmSteps[lmm]]
	},
	(LmmAlpha[lmm].SeriesVander[i, 0, p] - LmmBeta[lmm].SeriesVander[i, -1, p - 1]) / Last[LmmAlpha[lmm]]
];

LmmOrder[lmm_Lmm] := With[{
		a = LmmAlpha[lmm],
		b = LmmBeta[lmm],
		i = Range[0, LmmSteps[lmm]]
	},
	CountZeros[a.SeriesVander[i, #] - b.SeriesVander[i, # - 1] &] - 1
];

LmmErrorConstant[lmm_Lmm] := LmmErrorConstant[lmm, LmmOrder[lmm] + 1];
LmmErrorConstant[lmm_Lmm, p_Integer?NonNegative] := With[{
		i = Range[0, LmmSteps[lmm]]
	},
	(LmmAlpha[lmm].SeriesVander[i, p] - LmmBeta[lmm].SeriesVander[i, p - 1]) / LmmBetaGeneratingPolynomial[lmm, 1]
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
