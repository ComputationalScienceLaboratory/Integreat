(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`LMM`OrderConditions`"];

LMMOrderConditions::usage = "LMMOrderConditions[lmm, p] returns a list of order condition residuals for lmm from order 0 to p.  If a residual is zero, the corresponding order condition is satisfied.";
LMMOrder::usage = "LMMOrder[lmm] determines the order of lmm by evaluating order conditions.";
LMMErrorConstant::usage =
	"LMMErrorConstant[lmm] computes the leading error constant of lmm.\n" <>
	"LMMErrorConstant[lmm, p] computes the error constant of lmm assuming it is order p";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`Internal`MathUtils`", "Integreat`LMM`Core`"}];


(* ::Section:: *)
(*Package Definitions*)


LMMOrderConditions[lmm_LMM, p_Integer?NonNegative] := With[{
		i = Range[0, LMMSteps[lmm]]
	},
	(LMMAlpha[lmm] . SeriesVander[i, 0, p] - LMMBeta[lmm] . SeriesVander[i, -1, p - 1]) / Last[LMMAlpha[lmm]]
];

LMMOrder[lmm_LMM] := With[{
		a = LMMAlpha[lmm],
		b = LMMBeta[lmm],
		i = Range[0, LMMSteps[lmm]]
	},
	CountZeros[a . SeriesVander[i, #] - b . SeriesVander[i, # - 1] &, 0] - 1
];

LMMErrorConstant[lmm_LMM] := LMMErrorConstant[lmm, LMMOrder[lmm] + 1];
LMMErrorConstant[lmm_LMM, p_Integer?NonNegative] := With[{
		i = Range[0, LMMSteps[lmm]]
	},
	(LMMAlpha[lmm] . SeriesVander[i, p] - LMMBeta[lmm] . SeriesVander[i, p - 1]) / LMMBetaGeneratingPolynomial[lmm, 1]
];


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
