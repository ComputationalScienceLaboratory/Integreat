(* ::Package:: *)

(* ::Section:: *)
(*Package Definitions*)


LMMOrderConditions[lmm_LMM, {p_Integer?NonNegative}] := With[{
		i = Range[0, LMMSteps[lmm]]
	},
	(LMMAlpha[lmm] . SeriesVDM[i, p] - LMMBeta[lmm] . SeriesVDMs[i, p - 1]) / LMMBetaGeneratingPolynomial[lmm, 1]
];
LMMOrderConditions[lmm_LMM, p_Integer?NonNegative] := With[{
		v = SeriesVDM[Range[0, LMMSteps[lmm]], -1, p]
	},
	(LMMAlpha[lmm] . v[[All, 2;;]] - LMMBeta[lmm] . v[[All, ;;-2]]) / LMMBetaGeneratingPolynomial[lmm, 1]
];


LMMOrder[lmm_LMM] := CountZeros[LMMOrderConditions[lmm, {#}] &, 0] - 1;


LMMErrorConstant[lmm_LMM] := LMMErrorConstant[lmm, LMMOrder[lmm] + 1];
LMMErrorConstant[lmm_LMM, p_Integer?NonNegative] := LMMOrderConditions[lmm, {p}];
