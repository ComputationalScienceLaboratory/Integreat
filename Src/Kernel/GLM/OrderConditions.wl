(* ::Package:: *)

(* ::Section:: *)
(*Usage*)


BeginPackage["Integreat`GLM`OrderConditions`"];

GLMPreconsistencyConditions::usage = "GLMPreconsistencyConditions[glm] returns the residuals of the preconsistency conditions for glm.";
GLMOrderConditions::usage =
	"GLMOrderConditions[glm, {p, q}] generates order conditions residuals of glm up to order p and stage order q.\n" <>
	"GLMOrderConditions[glm, q] uses stage order q and p=GLMP[glm].\n" <>
	"GLMOrderConditions[glm] uses p=q=GLMP[glm].";
GLMOrder::usage = "GLMOrder[glm] computes the order of accuracy of glm assumping high stage order.";
GLMStageOrder::usage = "GLMStageOrder[glm] computes the order of accuracy of the internal stages of glm assumping high stage order.";


(* ::Section:: *)
(*Private Members*)


Begin["`Private`"];
Scan[Needs, {"Integreat`Internal`MathUtils`", "Integreat`GLM`Core`"}];

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


(* ::Section:: *)
(*End Package*)


End[];
EndPackage[];
