(* ::Package:: *)

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
