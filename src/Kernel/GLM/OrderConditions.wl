(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


glmStageOrderCond[glm_, q_] := SeriesVDM[GLMC[glm], q] - GLMA[glm] . SeriesVDM[GLMC[glm], q - 1] - GLMU[glm] . GLMQ[glm][[All, q + 1]];


glmOrderCond[glm_, p_] := GLMQ[glm][[All, ;;p + 1]] . (1 / Factorial[Range[p, 0, -1]]) - GLMB[glm] . SeriesVDM[GLMC[glm], p - 1] - GLMV[glm] . GLMQ[glm][[All, p + 1]];


glmOrder[glm_] := {
	CountZeros[glmStageOrderCond[glm, #] &, 0, GLMP[glm] + 1],
	CountZeros[glmOrderCond[glm, #] &, 0, GLMP[glm] + 1]
};


(* ::Section:: *)
(*Package Definitions*)


GLMOrderConditions[glm_GLM] := GLMOrderConditions[glm, GLMP[glm], GLMP[glm]];
GLMOrderConditions[glm_GLM, q_Integer?NonNegative] /; q <= GLMP[glm] := GLMOrderConditions[glm, q, GLMP[glm]];
GLMOrderConditions[glm_GLM, {p_Integer?NonNegative}] /; p <= GLMP[glm] := {glmStageOrderCond[glm, p], glmOrderCond[glm, p]};
GLMOrderConditions[glm_GLM, q_Integer?NonNegative, p_Integer?NonNegative] /; p - 1 <= q <= p <= GLMP[glm] := With[{
		C = SeriesVDM[GLMC[glm], -1, q],
		Q = GLMQ[glm]
	},
	Flatten[{
		C[[All, 2;;]] - GLMA[glm] . C[[All, ;;q+1]] - GLMU[glm] . Q[[All, ;;q+1]],
		Q . ToeplitzMatrix[UnitVector[GLMP[glm] + 1, 1], 1 / Factorial[Range[0, p]]] - GLMB[glm] . C[[All, ;;p+1]] - GLMV[glm] . Q[[All, ;;p+1]]
	}, {{3}, {1}, {2}}]
];


GLMOrder[glm_GLM] := Min[glmOrder[glm] - {0, 1}];


GLMStageOrder[glm_GLM] := Min[glmOrder[glm]] - 1;
