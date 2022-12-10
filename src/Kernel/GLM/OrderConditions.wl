(* ::Package:: *)

(* ::Section:: *)
(*Private Members*)


glmLteOrder[glm_GLM] := With[{
		p = GLMP[glm],
		c = GLMC[glm],
		Q = GLMQ[glm]
	},
	{
		CountZeros[SeriesVander[c, #] - GLMA[glm] . SeriesVander[c, # - 1] - GLMU[glm] . Q[[All, # + 1]] &, 0, p + 1],
		CountZeros[Q . Table[If[# < i, 0, 1 / Factorial[# - i]], {i, 0, p}] - GLMB[glm] . SeriesVander[c, # - 1] - GLMV[glm] . Q[[All, # + 1]] &, 0, p + 1]
	}
];


(* ::Section:: *)
(*Package Definitions*)


GLMPreconsistencyConditions[glm_GLM] := With[{
		q0 = GLMQ[glm][[All, 1]]
	},
	{
		GLMU[glm] . q0 - 1,
		GLMV[glm] . q0 - q0
	}
];


GLMOrderConditions[glm_GLM] := GLMOrderConditions[glm, GLMP[glm]];
GLMOrderConditions[glm_GLM, q_Integer?NonNegative] := GLMOrderConditions[glm, {GLMP[glm], q}];
GLMOrderConditions[glm_GLM, {p_Integer?NonNegative, q_Integer?NonNegative}] /; p <= GLMP[glm] && (p == q || p == q + 1) := With[{
		C = SeriesVander[GLMC[glm], -1, p],
		Q = GLMQ[glm]
	},
	Flatten[{
		C[[All, 2;;q+2]] - GLMA[glm] . C[[All, 1;;q+1]] - GLMU[glm] . Q[[All, 1;;q+1]],
		Q . Table[If[j < i, 0, 1 / Factorial[j - i]], {i, 0, p}, {j, 0, p}] - GLMB[glm] . C[[All, 1;;p+1]] - GLMV[glm] . Q
	}, {{3}, {1}, {2}}]
];


GLMStageOrder[glm_GLM] := Min[glmLteOrder[glm]] - 1;


GLMOrder[glm_GLM] := Min[glmLteOrder[glm] - {0, 1}];
